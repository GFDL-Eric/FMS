!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

program test

  use           fms_mod, only: fms_init, fms_end, check_nml_error
  use           fms_mod, only: error_mesg, field_exist, field_size
  use        fms_io_mod, only: read_data, fms_io_exit
  use  fms_affinity_mod, only: fms_affinity_set
  use  time_manager_mod, only: time_type
  use  diag_manager_mod, only: diag_manager_init, diag_manager_end, register_static_field, register_diag_field
  use  diag_manager_mod, only: send_data, diag_axis_init
  use data_override_mod, only: data_override_init, data_override, data_override_UG
  use           mpp_mod, only: mpp_pe, mpp_npes, mpp_root_pe, mpp_error, FATAL, NOTE
  use           mpp_mod, only: input_nml_file, stdout, stderr, mpp_chksum
  use           mpp_mod, only: mpp_sync_self, mpp_broadcast
  use   mpp_domains_mod, only: domain2d, mpp_define_domains, mpp_define_io_domain, mpp_define_layout
  use   mpp_domains_mod, only: domain2D, mpp_define_mosaic, cyclic_global_domain
  use   mpp_domains_mod, only: mpp_get_compute_domain, mpp_get_compute_domains, mpp_get_data_domain
  use   mpp_domains_mod, only: domainUG, mpp_define_unstruct_domain
  use   mpp_domains_mod, only: mpp_get_UG_compute_domain, mpp_pass_SG_to_UG, mpp_pass_UG_to_SG

  implicit none
 
  type dataOverrideVariable_t
    character(len=128)   :: varname
    character(len=3)     :: grid
    real, allocatable    :: array(:,:)
    real                 :: before_sum
    real                 :: after
    logical, allocatable :: override(:)
  end type dataOverrideVariable_t

  integer                           :: nthreads=1
  character(len=256)                :: varname='sst_obs'
  character(len=3)                  :: gridname='OCN'
  integer                           :: omp_get_num_threads
  integer                           :: isw, iew, jsw, jew
  integer, allocatable              :: is_win(:), js_win(:)
  integer                           :: nx_dom, ny_dom, nx_win, ny_win
  type(domain2d)                    :: Domain
  integer                           :: nlon, nlat
  real, allocatable, dimension(:,:) :: lon, lat
  integer                           :: i, j, is, ie, js, je, io, ierr, n
  character(len=256)                :: tile_file
  type(time_type)                   :: Time
  integer, dimension(2)             :: layout = (/0,0/)
  integer                           :: window(2) = (/1,1/)
  integer                           :: nwindows
  type(dataOverrideVariable_t)      :: vardo
  namelist / test_data_override_nml / layout, window, nthreads
 
  read (input_nml_file, test_data_override_nml, iostat=io)
  ierr = check_nml_error(io, 'test_data_override_nml')

  call fms_init

  call get_nlon_nlat(tile_file, nlon, nlat)

  if(layout(1)*layout(2) .NE. mpp_npes() ) then
    call mpp_define_layout( (/1,nlon,1,nlat/), mpp_npes(), layout )
  end if
 
  call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_data_override')
  call mpp_define_io_domain(Domain, (/1,1/))
  call data_override_init(Ice_domain_in=Domain, Ocean_domain_in=Domain)
  call mpp_get_compute_domain(Domain, is, ie, js, je)
  call get_grid

  Time = set_date(2000,7,1,0,0,0)
 
  nx_dom = ie - is + 1
  ny_dom = je - js + 1
  if( mod( nx_dom, window(1) ) .NE. 0 ) call error_mesg('test_data_override', &
          "nx_dom is not divisible by window(1)", FATAL)
  if( mod( ny_dom, window(2) ) .NE. 0 ) call error_mesg('test_data_override', &
        "ny_dom is not divisible by window(2)", FATAL)

  nwindows = window(1)*window(2)

  nx_win = nx_dom/window(1)
  ny_win = ny_dom/window(2)
  allocate(is_win(nwindows), js_win(nwindows))

  i = 1
  do jsw = js,je,ny_win
    do isw = is,ie,nx_win
      is_win(m) = isw
      js_win(m) = jsw
      i = i + 1
    end do
  end do

  vardo = construct_data_override_variable(gridname, varname, nwindows)

!$ call omp_set_num_threads(nthreads)
!$OMP PARALLEL
!$ call fms_affinity_set("test_data_override", .FALSE., omp_get_num_threads() )
!$OMP END PARALLEL

!$OMP parallel do schedule(static) default(shared) private(isw, iew, jsw, jew)
  do n = 1, nwindows
    isw = is_win(n)
    iew = isw + nx_win - 1
    jsw = js_win(n)
    jew = jsw + ny_win - 1
    call data_override(vardo%grid, trim(vardo%varname), vardo%array(isw:iew,jsw:jew), Time, override=vardo%override(n), &
                      is_in=isw-is+1, ie_in=iew-is+1, js_in=jsw-js+1, je_in=jew-js+1)
  enddo

  call check_override_fails(vardo)
  call print_before_sums(vardo)
  call calc_after_sum(vardo)
  call print_after_sums(vardo)
  call check_sums_equal(vardo)

  call send_data_data_override

  call destruct_data_override_variable(vardo)

  call fms_io_exit
  call fms_end

contains

  pure function construct_data_override_variable(gridname, varname, nwindows) result(vardo)
    character(len=*), intent(in) :: gridname
    character(len=*), intent(in) :: varname
    integer, intent(in)          :: nwindows
    type(dataOverrideVariable_t) :: vardo

    vardo%grid = trim(gridname)
    vardo%varname = trim(varname)
    allocate(vardo%array(is:ie,js:je))
    vardo%array = 1.0
    vardo%before = SUM(vardo%array)
    allocate(vardo%override(nwindows))
  end function constuct_data_override_variable

  subroutine destruct_data_override_variable(vardo)
    type(dataOverrideVariable_t) :: vardo

    deallocate(vardo%array)
    deallocate(vardo%override(nwindows))
  end subroutine destuct_data_override_variable

  subroutine calc_after_sum(vardo)
    type(dataOverrideVariable_t), intent(inout) :: vardo
    vardo%after = SUM(vardo%array)
  end subroutine calc_after_sum

  subroutine print_before_sums(vardo)
    type(dataOverrideVariable_t), intent(inout) :: vardo
    print *, trim(vardo%varname)//" sum before override", vardo%before
    print *, trim(vardo%varname)//" checksum before override", mpp_chksum(vardo%array)
  end subroutine print_before_sums

  subroutine print_after_sums(vardo)
    type(dataOverrideVariable_t), intent(inout) :: vardo
    print *, trim(vardo%varname)//" sum after override", vardo%after
    print *, trim(vardo%varname)//" checksum after override", mpp_chksum(vardo%array)
  end subroutine print_after_sums

  subroutine check_sums_equal(vardo)
    type(dataOverrideVariable_t), intent(inout) :: vardo
    if(vardo%after == vardo%before) then
      call error_mesg('test_data_override', vardo%varname//' sums before and after override are equal', FATAL)
    endif
  end subroutine check_sums_equal

  subroutine check_override_fails(vardo)
    type(dataOverrideVariable_t), intent(inout) :: vardo
    character(len=128)                          :: message
    if(ANY(.NOT. vardo%override)) then
      message = 'override failed for '//trim(vardo%varname)
    endif
    call error_mesg('test_data_override', trim(message), FATAL)
  end subroutine check_override_fails

  subroutine send_data_data_override
    real, allocatable, dimension(:)   :: x, y
    integer                           :: id_x, id_y, id_lon, id_lat, id_var1, id_var2, id_var3
    logical                           :: used

    call diag_manager_init
    allocate(x(nlon), y(nlat))
   
    do i=1,nlon
      x(i) = i
    enddo
    do j=1,nlat
      y(j) = j
    enddo
  
    id_x  = diag_axis_init('x', x, 'point_E', 'x', long_name='point_E', Domain2=Domain)
    id_y  = diag_axis_init('y', y, 'point_N', 'y', long_name='point_N', Domain2=Domain)
   
    id_lon = register_static_field('test_data_override_mod', 'lon', (/id_x,id_y/), 'longitude', 'Degrees')
    id_lat = register_static_field('test_data_override_mod', 'lat', (/id_x,id_y/), 'latitude', 'Degrees')
    id_var1 = register_diag_field('test_data_override_mod', 'var1', (/id_x,id_y/), Time, 'var1', ' ')
    id_var2 = register_diag_field('test_data_override_mod', 'var2', (/id_x,id_y/), Time, 'var2', ' ')
    id_var3 = register_diag_field('test_data_override_mod', 'var3', (/id_x,id_y/), Time, 'var3', ' ')
  
    used = send_data(id_lon, lon, Time)
    used = send_data(id_lat, lat, Time)
    if(id_var1 > 0) used = send_data(id_var1, var1, Time)
    if(id_var2 > 0) used = send_data(id_var2, var2, Time)
    if(id_var3 > 0) used = send_data(id_var3, var3, Time)

    deallocate(x, y)
    call diag_manager_end(Time)

  end subroutine send_data_data_override 
  subroutine get_nlon_nlat(tile_file, nlon, nlat, grid_file)
    character(len=128), intent(in), optional :: grid_file
    character(len=256), intent(out)          :: tile_file
    integer, intent(out)                     :: nlon, nlat
    character(len=256)                       :: solo_mosaic_file
    integer, dimension(4)                    :: siz

    if (.not. present(grid_file)) then
        grid_file = "INPUT/grid_spec.nc"
    end if
    if(field_exist(grid_file, "x_T" ) ) then
      call field_size(grid_file, 'x_T', siz)
      nlon = siz(1)
      nlat = siz(2)
      tile_file = repeat(' ',len(tile_file))
    else if(field_exist(grid_file, "geolon_t" ) ) then
      call field_size(grid_file, 'geolon_t', siz)
      nlon = siz(1)-1
      nlat = siz(2)-1
      tile_file = repeat(' ',len(tile_file))
    else if (field_exist(grid_file, "ocn_mosaic_file" )) then
      call read_data(grid_file, 'ocn_mosaic_file', solo_mosaic_file)
      solo_mosaic_file = 'INPUT/'//trim(solo_mosaic_file)
      call field_size(solo_mosaic_file, 'gridfiles', siz)
      if( siz(2) .NE. 1) call error_mesg('test_data_override', 'only support single tile mosaic, contact developer', FATAL)
      call read_data(solo_mosaic_file, 'gridfiles', tile_file)
      tile_file = 'INPUT/'//trim(tile_file)
      call field_size(tile_file, 'area', siz)
      if(mod(siz(1),2) .NE. 0 .OR. mod(siz(2),2) .NE. 0 ) then
        call error_mesg('test_data_override',"test_data_override: supergrid size can not be divided by 2", FATAL)
      end if
      nlon = siz(1)/2
      nlat = siz(2)/2
    else
      call error_mesg('test_data_override', 'x_T, geolon_t and ocn_mosaic_file does not exist', FATAL)
    end if
  end subroutine get_nlon_nlat

  subroutine get_grid
    real, allocatable, dimension(:,:,:) :: lon_vert_glo, lat_vert_glo
    real, allocatable, dimension(:,:)   :: lon_global, lat_global
    character(len=128) :: message

    if(field_exist(grid_file, 'x_T')) then
      allocate(lon_vert_glo(nlon,nlat,4), lat_vert_glo(nlon,nlat,4) )
      allocate(lon_global  (nlon,nlat  ), lat_global  (nlon,nlat  ) )
      call read_data(trim(grid_file), 'x_vert_T', lon_vert_glo, no_domain=.true.)
      call read_data(trim(grid_file), 'y_vert_T', lat_vert_glo, no_domain=.true.)
      lon_global(:,:)  = (lon_vert_glo(:,:,1) + lon_vert_glo(:,:,2) + lon_vert_glo(:,:,3) + lon_vert_glo(:,:,4))*0.25
      lat_global(:,:) =  (lat_vert_glo(:,:,1) + lat_vert_glo(:,:,2) + lat_vert_glo(:,:,3) + lat_vert_glo(:,:,4))*0.25
    else if(field_exist(grid_file, "geolon_t")) then
      allocate(lon_vert_glo(nlon+1,nlat+1,1), lat_vert_glo(nlon+1,nlat+1,1))
      allocate(lon_global  (nlon,  nlat    ), lat_global  (nlon,  nlat    ))
      call read_data(trim(grid_file), 'geolon_vert_t', lon_vert_glo, no_domain=.true.)
      call read_data(trim(grid_file), 'geolat_vert_t', lat_vert_glo, no_domain=.true.)

      do i = 1, nlon
        do j = 1, nlat
          lon_global(i,j) = (lon_vert_glo(i,j,1) + lon_vert_glo(i+1,j,1) + &
            lon_vert_glo(i+1,j+1,1) + lon_vert_glo(i,j+1,1))*0.25
          lat_global(i,j) = (lat_vert_glo(i,j,1) + lat_vert_glo(i+1,j,1) + &
            lat_vert_glo(i+1,j+1,1) + lat_vert_glo(i,j+1,1))*0.25
        enddo
      enddo
    else if(field_exist(grid_file, "ocn_mosaic_file")) then
      allocate(lon_vert_glo(nlon*2+1,nlat*2+1,1), lat_vert_glo(nlon*2+1,nlat*2+1,1))
      allocate(lon_global  (nlon,  nlat    ), lat_global  (nlon,  nlat    ))
      call read_data( tile_file, 'x', lon_vert_glo, no_domain=.true.)
      call read_data( tile_file, 'y', lat_vert_glo, no_domain=.true.)
      do j = 1, nlat
        do i = 1, nlon
          lon_global(i,j) = lon_vert_glo(i*2,j*2,1)
          lat_global(i,j) = lat_vert_glo(i*2,j*2,1)
        end do
      end do
    end if

    allocate(lon(is:ie,js:je), lat(is:ie,js:je))
    lon = lon_global(is:ie,js:je)
    lat = lat_global(is:ie,js:je)

    deallocate(lon_vert_glo)
    deallocate(lat_vert_glo)
    deallocate(lon_global)
    deallocate(lat_global)

  end subroutine get_grid

  subroutine compare_checksums( a, b, string )
    real, intent(in), dimension(:,:,:) :: a, b
    character(len=*), intent(in) :: string
    integer(8) :: sum1, sum2
    integer :: i, j, k, pe

    call mpp_sync_self()
    pe = mpp_pe()

    if(size(a,1) .ne. size(b,1) .or. size(a,2) .ne. size(b,2) .or. size(a,3) .ne. size(b,3) ) then
      call mpp_error(FATAL,'compare_chksum: size of a and b does not match')
    end if

    do k = 1, size(a,3)
      do j = 1, size(a,2)
        do i = 1, size(a,1)
          if(a(i,j,k) .ne. b(i,j,k)) then
            print*, "pe,i,j,k", pe,i,j,k
            print*, "a =", a(i,j,k)
            print*, "b =", b(i,j,k)
            call mpp_error(FATAL, trim(string)//': point by point comparison are not OK.')
          endif
        enddo
      enddo
    enddo

    sum1 = mpp_chksum( a, (/pe/) )
    sum2 = mpp_chksum( b, (/pe/) )

    if(sum1.EQ.sum2) then
      if(pe.EQ.mpp_root_pe()) then
        call mpp_error(NOTE, trim(string)//': OK.')
      end if
    else
      call mpp_error(FATAL, trim(string)//': chksums are not OK.')
    end if
  end subroutine compare_checksums

  subroutine compare_checksums_2D( a, b, string )
    real, intent(in), dimension(:,:) :: a, b
    character(len=*), intent(in) :: string
    integer(8) :: sum1, sum2
    integer :: i, j, pe

    call mpp_sync_self()
    pe = mpp_pe()

    if(size(a,1) .ne. size(b,1) .or. size(a,2) .ne. size(b,2)) then
      call mpp_error(FATAL,'compare_chksum_2D: size of a and b does not match')
    end if

    do j = 1, size(a,2)
      do i = 1, size(a,1)
        if(a(i,j) .ne. b(i,j)) then
          print*, "i,j= ", i,j
          print*, "a =", a(i,j)
          print*, "b =", b(i,j)
          call mpp_error(FATAL, trim(string)//': point by point comparison are not OK.')
        endif
      enddo
    enddo

    sum1 = mpp_chksum(a, (/pe/))
    sum2 = mpp_chksum(b, (/pe/))

    if(sum1.EQ.sum2) then
      if(pe.EQ.mpp_root_pe()) then
        call mpp_error(NOTE, trim(string)//': OK.')
      end if
    else
      call mpp_error(FATAL, trim(string)//': chksums are not OK.')
    end if
  end subroutine compare_checksums_2D

end program test
