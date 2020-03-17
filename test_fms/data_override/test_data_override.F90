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
module class_gridStyle
  use     fms_mod, only: error_mesg
  use     mpp_mod, only: mpp_error, FATAL
  use fms2_io_mod, only: FmsNetcdfFile_t, get_variable_size
  use fms2_io_mod, only: open_file, read_data, close_file, variable_exists

  implicit none
  private

  integer                 :: i, j

  type, public :: gridStyle_t
    character(len=32)     :: gridstyle
    character(len=16)     :: var
    type(FmsNetcdfFile_t) :: obj
    integer               :: nlon
    integer               :: nlat
    real, allocatable     :: lon_global(:,:)
    real, allocatable     :: lat_global(:,:)
    contains
      procedure           :: get_nlon_nlat    => calc_nlon_nlat
      procedure           :: get_grid_globals => get_lonlat_globals
      procedure           :: destruct         => destruct_grid_style
  end type gridStyle_t

  interface gridStyle_t
    module procedure construct_grid_style
  end interface gridStyle_t
contains

  subroutine calc_nlon_nlat(this)
    class(gridStyle_t), intent(inout) :: this
    integer, dimension(2)          :: siz

    call get_variable_size(this%obj, trim(this%var), siz)
    this%nlon = siz(1)
    this%nlat = siz(2)
    if(trim(this%gridstyle) == "geolon_t") then
      this%nlon = this%nlon-1
      this%nlat = this%nlat-1
    else if(trim(this%gridstyle) == "ocn_mosaic_file") then
      if(mod(this%nlon,2) .NE. 0 .OR. mod(this%nlat,2) .NE. 0) then
        call error_mesg('test_data_override',"test_data_override: supergrid size can not be divided by 2", FATAL)
      end if
      this%nlon = this%nlon/2
      this%nlat = this%nlat/2
    end if
  end subroutine calc_nlon_nlat

  function get_obj(file_name)
    character(len=128), intent(in) :: file_name
    type(FmsNetcdfFile_t)          :: get_obj
    if(.not. open_file(get_obj, file_name, 'read')) then
      call mpp_error(FATAL, 'test_data_override(get_obj):Error in opening file '//trim(file_name))
    endif
  end function get_obj

  function construct_grid_style(grid_file_name) result(this)
    character(len=128), intent(in), optional     :: grid_file_name
    type(gridStyle_t)                            :: this
    type(FmsNetcdfFile_t)                        :: gridobj, mosaicobj, tileobj
    character(len=256)                           :: solo_mosaic_file, tile_file, grid_file
    integer, dimension(2)                        :: grid_siz

    if (.not. present(grid_file_name)) then
      grid_file = "INPUT/grid_spec.nc"
    else
      grid_file = trim(grid_file_name)
    end if
    gridobj = get_obj(grid_file)
    if(variable_exists(gridobj, "x_T")) then
      this%gridstyle = 'x_T'
      this%obj = gridobj
      this%var = 'x_T'
    else if(variable_exists(gridobj, "geolon_t")) then
      this%gridstyle = 'geolon_t'
      this%obj = gridobj
      this%var = 'geolon_t'
    else if(variable_exists(gridobj, "ocn_mosaic_file")) then
      call read_data(gridobj, 'ocn_mosaic_file', solo_mosaic_file)
      solo_mosaic_file = 'INPUT/'//trim(solo_mosaic_file)
      mosaicobj = get_obj(solo_mosaic_file)
      call get_variable_size(mosaicobj, 'gridfiles', grid_siz)
      if( grid_siz(2) .NE. 1) call error_mesg('test_data_override', 'only support single tile mosaic, contact developer', FATAL)
      call read_data(mosaicobj, 'gridfiles', tile_file)
      tile_file = 'INPUT/'//trim(tile_file)
      tileobj = get_obj(tile_file)
      this%gridstyle = 'ocn_mosaic_file'
      this%obj = tileobj
      this%var = 'area'
      call close_file(mosaicobj)
      call close_file(gridobj)
    else
      call error_mesg('test_data_override', 'x_T, geolon_t and ocn_mosaic_file does not exist', FATAL)
    end if
  end function construct_grid_style

  subroutine destruct_grid_style(this)
    class(gridStyle_t), intent(inout) :: this

    call close_file(this%obj)
    if (allocated(this%lon_global)) deallocate(this%lon_global)
    if (allocated(this%lat_global)) deallocate(this%lat_global)
  end subroutine destruct_grid_style

  subroutine get_lonlat_globals(this)
    class(gridStyle_t), intent(inout)   :: this
    real, allocatable, dimension(:,:,:) :: lon_vert_glo, lat_vert_glo

    if(trim(this%gridstyle) == "x_T") then
      allocate(lon_vert_glo(this%nlon, this%nlat,4), lat_vert_glo(this%nlon, this%nlat,4))
      allocate(this%lon_global(this%nlon, this%nlat), this%lat_global(this%nlon, this%nlat))
      call read_data(this%obj, 'x_vert_T', lon_vert_glo)
      call read_data(this%obj, 'y_vert_T', lat_vert_glo)
      this%lon_global(:,:) = (lon_vert_glo(:,:,1) + lon_vert_glo(:,:,2) + lon_vert_glo(:,:,3) + lon_vert_glo(:,:,4))*0.25
      this%lat_global(:,:) = (lat_vert_glo(:,:,1) + lat_vert_glo(:,:,2) + lat_vert_glo(:,:,3) + lat_vert_glo(:,:,4))*0.25
    else if(trim(this%gridstyle) == "geolon_t") then
      allocate(lon_vert_glo(this%nlon+1, this%nlat+1, 1), lat_vert_glo(this%nlon+1, this%nlat+1,1))
      allocate(this%lon_global(this%nlon, this%nlat), this%lat_global(this%nlon, this%nlat))
      call read_data(this%obj, 'geolon_vert_t', lon_vert_glo)
      call read_data(this%obj, 'geolat_vert_t', lat_vert_glo)

      do i = 1, this%nlon
        do j = 1, this%nlat
          this%lon_global(i,j) = (lon_vert_glo(i,j,1) + lon_vert_glo(i+1,j,1) + &
            lon_vert_glo(i+1,j+1,1) + lon_vert_glo(i,j+1,1))*0.25
          this%lat_global(i,j) = (lat_vert_glo(i,j,1) + lat_vert_glo(i+1,j,1) + &
            lat_vert_glo(i+1,j+1,1) + lat_vert_glo(i,j+1,1))*0.25
        enddo
      enddo
    else if(trim(this%gridstyle) == "ocn_mosaic_file") then
      allocate(lon_vert_glo(this%nlon*2+1, this%nlat*2+1, 1), lat_vert_glo(this%nlon*2+1, this%nlat*2+1, 1))
      allocate(this%lon_global(this%nlon, this%nlat), this%lat_global(this%nlon, this%nlat))
      call read_data(this%obj, 'x', lon_vert_glo)
      call read_data(this%obj, 'y', lat_vert_glo)
      do j = 1, this%nlat
        do i = 1, this%nlon
          this%lon_global(i,j) = lon_vert_glo(i*2,j*2,1)
          this%lat_global(i,j) = lat_vert_glo(i*2,j*2,1)
        end do
      end do
    end if

    if (allocated(lon_vert_glo)) deallocate(lon_vert_glo)
    if (allocated(lat_vert_glo)) deallocate(lat_vert_glo)

  end subroutine get_lonlat_globals


end module class_gridStyle

module class_domain
  use            class_gridStyle
  use                    fms_mod, only: error_mesg
  use                    mpp_mod, only: mpp_npes, mpp_chksum, FATAL
  use            mpp_domains_mod, only: domain2d, mpp_define_domains, mpp_define_io_domain, mpp_define_layout
  use            mpp_domains_mod, only: mpp_get_compute_domain

  implicit none
  private

  type, public :: domainType_t
    type(domain2D)             :: domain
    type(gridStyle_t), pointer :: d_grid => null()
    integer                    :: layout(2)
    integer                    :: window(2)
    integer                    :: nx_dom
    integer                    :: nx_win
    integer                    :: ny_dom
    integer                    :: ny_win
    integer                    :: nwindows
    integer                    :: is
    integer                    :: ie
    integer                    :: js
    integer                    :: je
    real, allocatable          :: lon(:,:)
    real, allocatable          :: lat(:,:)
    contains
      procedure    :: adjust_layout   => adjust_layout
      procedure    :: get_all_domains => get_all_domains
      procedure    :: nxny            => nxny
      procedure    :: get_domain_grid => get_domain_grid
      procedure    :: destruct        => destruct_domain
  end type domainType_t

  interface domainType_t
    module procedure construct_domain_type
  end interface domainType_t

contains
  function construct_domain_type(d_grid, d_window, layout) result(this)
    type(gridStyle_t), intent(in), target :: d_grid
    integer, intent(in)                   :: d_window(2)
    integer, intent(in), optional         :: layout(2)
    type(domainType_t)                    :: this

    if(.not. present(layout)) then
      this%layout = (/0, 0/)
    else
      this%layout = layout
    endif
    this%window = d_window
    allocate(this%d_grid)
    this%d_grid => d_grid
  end function construct_domain_type

  subroutine destruct_domain(this)
    class(domainType_t), intent(inout) :: this

    if (allocated(this%lon)) deallocate(this%lon)
    if (allocated(this%lat)) deallocate(this%lat)
    this%d_grid => null()
    if (associated(this%d_grid)) deallocate(this%d_grid)
  end subroutine destruct_domain

  subroutine adjust_layout(this)
    class(domainType_t), intent(inout) :: this

    if(this%layout(1)*this%layout(2) .NE. mpp_npes() ) then
      call mpp_define_layout( (/1,this%d_grid%nlon,1,this%d_grid%nlat/), mpp_npes(), this%layout )
    end if
  end subroutine adjust_layout

  subroutine get_all_domains(this)
    class(domainType_t), intent(inout) :: this

    call mpp_define_domains((/1,this%d_grid%nlon,1,this%d_grid%nlat/), this%layout, this%domain, name='test_data_override')
    call mpp_define_io_domain(this%domain, (/1,1/))
    call mpp_get_compute_domain(this%domain, this%is, this%ie, this%js, this%je)
  end subroutine get_all_domains

  subroutine nxny(this)
    class(domainType_t), intent(inout) :: this

    this%nx_dom = this%ie - this%is + 1
    this%ny_dom = this%je - this%js + 1
    if( mod( this%nx_dom, this%window(1) ) .NE. 0 ) call error_mesg('test_data_override', &
            "nx_dom is not divisible by window(1)", FATAL)
    if( mod( this%ny_dom, this%window(2) ) .NE. 0 ) call error_mesg('test_data_override', &
          "ny_dom is not divisible by window(2)", FATAL)

    this%nwindows = this%window(1)*this%window(2)

    this%nx_win = this%nx_dom/this%window(1)
    this%ny_win = this%ny_dom/this%window(2)
  end subroutine nxny

  subroutine get_domain_grid(this)
    class(domainType_t), intent(inout) :: this

    allocate(this%lon(this%is:this%ie,this%js:this%je), this%lat(this%is:this%ie,this%js:this%je))
    this%lon = this%d_grid%lon_global(this%is:this%ie,this%js:this%je)
    this%lat = this%d_grid%lat_global(this%is:this%ie,this%js:this%je)
  end subroutine get_domain_grid

end module class_domain

module class_dataOverrideVariable
  use      class_domain
  use           fms_mod, only: error_mesg
  use           mpp_mod, only: mpp_chksum, FATAL
  use data_override_mod, only: data_override_init, data_override
  use   mpp_domains_mod, only: domain2d
  use  time_manager_mod, only: time_type, set_date, set_calendar_type, NOLEAP
  use  diag_manager_mod, only: diag_manager_init, diag_manager_end, register_static_field, register_diag_field
  use  diag_manager_mod, only: send_data, diag_axis_init

  implicit none
  private

  type, public :: dataOverrideVariable_t
    character(len=128)          :: varname
    character(len=3)            :: grid
    type(domainType_t), pointer :: domain => null()
    real, allocatable           :: array(:,:)
    real                        :: before
    real                        :: after
    logical, allocatable        :: override(:)
    type(time_type)             :: time
    contains
      procedure                 :: destruct                  => destruct_data_override_variable
      procedure                 :: get_time                  => get_time
      procedure                 :: calc_after_sum            => calc_after_sum
      procedure                 :: print_before_sums         => print_before_sums
      procedure                 :: print_after_sums          => print_after_sums
      procedure                 :: check_sums_equal          => check_sums_equal
      procedure                 :: check_override_fails      => check_override_fails
      procedure                 :: data_override_init_wrap2D => data_override_init_wrap2D
      procedure                 :: exec_data_override        => exec_data_override
      procedure                 :: send_data_do              => send_data_do
  end type dataOverrideVariable_t

  interface dataOverrideVariable_t
    module procedure construct_data_override_variable
  end interface dataOverrideVariable_t

contains

  function construct_data_override_variable(do_gridname, do_domain, do_varname) result(this)
    character(len=3), intent(in)           :: do_gridname
    character(len=128), intent(in)         :: do_varname
    type(domainType_t), intent(in), target :: do_domain
    type(dataOverrideVariable_t)           :: this

    allocate(this%domain)
    this%domain => do_domain
    this%grid = trim(do_gridname)
    this%varname = trim(do_varname)
    allocate(this%array(this%domain%is:this%domain%ie,this%domain%js:this%domain%je))
    this%array = 1.0
    this%before = SUM(this%array)
    allocate(this%override(this%domain%nwindows))
  end function construct_data_override_variable

  subroutine destruct_data_override_variable(this)
    class(dataOverrideVariable_t) :: this

    if (allocated(this%array)) deallocate(this%array)
    if (allocated(this%override)) deallocate(this%override)
    this%domain => null()
    if (associated(this%domain)) deallocate(this%domain)
  end subroutine destruct_data_override_variable

  subroutine get_time(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    call set_calendar_type(NOLEAP)
    this%time = set_date(2000,7,1,0,0,0)
  end subroutine get_time

  subroutine calc_after_sum(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    this%after = SUM(this%array)
  end subroutine calc_after_sum

  subroutine print_before_sums(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    print *, trim(this%varname)//" sum before override", this%before
    print *, trim(this%varname)//" checksum before override", mpp_chksum(this%array)
  end subroutine print_before_sums

  subroutine print_after_sums(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    print *, trim(this%varname)//" sum after override", this%after
    print *, trim(this%varname)//" checksum after override", mpp_chksum(this%array)
  end subroutine print_after_sums

  subroutine check_sums_equal(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    if(this%after == this%before) then
      call error_mesg('test_data_override', this%varname//' sums before and after override are equal', FATAL)
    endif
  end subroutine check_sums_equal

  subroutine check_override_fails(this)
    class(dataOverrideVariable_t), intent(inout) :: this
    character(len=128)                           :: message
    
    if(ANY(.NOT. this%override)) then
      message = 'override failed for '//trim(this%varname)
      call error_mesg('test_data_override', trim(message), FATAL)
    endif
  end subroutine check_override_fails

  subroutine data_override_init_wrap2D(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    if(this%grid == 'ICE') then
      call data_override_init(Ice_domain_in=this%domain%domain)
    else if(this%grid == 'OCN') then
      call data_override_init(Ocean_domain_in=this%domain%domain)
    else if(this%grid == 'ATM') then
      call data_override_init(Atm_domain_in=this%domain%domain)
    else if(this%grid == 'LND') then
      call data_override_init(Land_domain_in=this%domain%domain)
    end if
  end subroutine data_override_init_wrap2D

  subroutine exec_data_override(this)
    class(dataOverrideVariable_t), intent(inout) :: this

    integer                                      :: isw, iew, jsw, jew, n

    call set_calendar_type(NOLEAP)
!$OMP parallel do schedule(static) default(shared) private(isw, iew, jsw, jew)
    do n = 1, this%domain%nwindows
      isw = this%domain%nx_win*mod(n-1,this%domain%window(1)) + this%domain%is
      iew = isw + this%domain%nx_win - 1
      jsw = this%domain%ny_win*((n-1)/this%domain%window(1)) + this%domain%js
      jew = jsw + this%domain%ny_win - 1
      call data_override(this%grid, trim(this%varname), this%array(isw:iew,jsw:jew), this%time, override=this%override(n), &
          is_in=isw-this%domain%is+1, ie_in=iew-this%domain%is+1, js_in=jsw-this%domain%js+1, je_in=jew-this%domain%js+1)
    enddo
  end subroutine exec_data_override

  subroutine send_data_do(this)
    class(dataOverrideVariable_t), intent(inout) :: this
    real, allocatable, dimension(:)              :: x, y
    integer                                      :: i, j, id_x, id_y, id_lon, id_lat, id_var1
    logical                                      :: used

    call diag_manager_init
    call set_calendar_type(NOLEAP)
    allocate(x(this%domain%d_grid%nlon), y(this%domain%d_grid%nlat))
   
    do i=1,this%domain%d_grid%nlon
      x(i) = i
    enddo
    do j=1,this%domain%d_grid%nlat
      y(j) = j
    enddo
  
    id_x  = diag_axis_init('x', x, 'point_E', 'x', long_name='point_E', Domain2=this%domain%domain)
    id_y  = diag_axis_init('y', y, 'point_N', 'y', long_name='point_N', Domain2=this%domain%domain)
   
    id_lon = register_static_field('test_data_override_mod', 'lon', (/id_x,id_y/), 'longitude', 'Degrees')
    id_lat = register_static_field('test_data_override_mod', 'lat', (/id_x,id_y/), 'latitude', 'Degrees')
    id_var1 = register_diag_field('test_data_override_mod', trim(this%varname), (/id_x,id_y/), this%time, trim(this%varname), ' ')
  
    used = send_data(id_lon, this%domain%lon, this%time)
    used = send_data(id_lat, this%domain%lat, this%time)
    if(id_var1 > 0) used = send_data(id_var1, this%array, this%time)

    if (allocated(x) .and. allocated(y)) deallocate(x, y)
    call diag_manager_end(this%time)

  end subroutine send_data_do 


end module class_dataOverrideVariable


program test

  use               class_domain
  use            class_gridStyle
  use class_dataOverrideVariable
  use                    fms_mod, only: fms_init, fms_end, check_nml_error
  use           fms_affinity_mod, only: fms_affinity_set
  use                    mpp_mod, only: input_nml_file

  implicit none
 
  integer                           :: nthreads=1
  character(len=256)                :: varname='sst_obs'
  character(len=3)                  :: gridname='OCN'
  integer                           :: omp_get_num_threads
  integer, dimension(2)             :: layout = (/0,0/)
  integer                           :: window(2) = (/1,1/)
  integer                           :: testnum
  integer                           :: io, ierr
  type(dataOverrideVariable_t)      :: vardo
  type(domainType_t)                :: my_domain
  type(gridStyle_t)                 :: my_grid
  namelist / test_data_override_nml / varname, gridname, testnum, window
 
  call fms_init
  read (input_nml_file, test_data_override_nml, iostat=io)
  ierr = check_nml_error(io, 'test_data_override_nml')

  print *, "gridname", gridname
  print *, "varname", varname
  print *, "testnum", testnum
  print *, "window", window

  my_grid = gridStyle_t()
  call my_grid%get_nlon_nlat
  call my_grid%get_grid_globals

  my_domain = domainType_t(my_grid, window)
  call my_domain%adjust_layout
  call my_domain%get_all_domains
  call my_domain%nxny
  call my_domain%get_domain_grid

  vardo = dataOverrideVariable_t(gridname, my_domain, varname)
  call vardo%print_before_sums
  call vardo%data_override_init_wrap2D

!$ call omp_set_num_threads(nthreads)
!!! !$OMP PARALLEL
!!! !$ call fms_affinity_set("test_data_override", .FALSE., omp_get_num_threads() )
!!! !$OMP END PARALLEL

  call vardo%exec_data_override
  call vardo%check_override_fails
  call vardo%calc_after_sum
  call vardo%print_after_sums
  call vardo%check_sums_equal

  call vardo%send_data_do

  call vardo%destruct
  call my_domain%destruct
  call my_grid%destruct
  call fms_end

end program test
