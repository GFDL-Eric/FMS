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
!> @author Ryan Mulhall
!> @email gfdl.climate.model.info@noaa.gov
!> @brief Unit test for mpp_global_ max and min
!> @description Test of mpp_global_max and mpp_global_min interfaces
!> with mixed size integers and reals
program test_global_minmax

  use platform_mod
  use mpp_mod,         only: mpp_init, mpp_exit, mpp_pe, mpp_npes, mpp_root_pe
  use mpp_mod,         only: mpp_set_stack_size, mpp_sync, mpp_sync_self
  use mpp_mod,         only: mpp_error, FATAL, NOTE, mpp_send, mpp_recv
  use mpp_mod,         only: mpp_init_test_init_true_only
  use mpp_io_mod,      only: mpp_io_init
  use mpp_domains_mod, only: mpp_domains_init, mpp_define_domains, domain2d
  use mpp_domains_mod, only: mpp_define_layout, mpp_domains_set_stack_size
  use mpp_domains_mod, only: mpp_get_global_domain, mpp_global_max
  use mpp_domains_mod, only: mpp_global_min, mpp_get_data_domain,mpp_get_compute_domain
  use mpp_domains_mod, only: mpp_domains_exit, mpp_update_domains
  use mpp_domains_mod, only: mpp_get_domain_shift


  implicit none
    
  integer                       :: length=64
  integer                       :: id, pe, npes, root, i, j
  integer(i4_kind)              :: maxI4, minI4,ierr
  integer(i8_kind)              :: maxI8, minI8
  integer(i4_kind), allocatable :: dataI4(:,:)
  integer(i8_kind), allocatable :: dataI8(:,:)
  real(r4_kind), allocatable    :: dataR4(:,:)
  real(r8_kind), allocatable    :: dataR8(:,:)
  real, allocatable             :: rands(:)
  type(domain2D)                :: domain
  real(r8_kind)                 :: rcoef, maxR8,minR8
  real(r4_kind)                 :: maxR4, minR4
  integer                       :: isc, iec, jsc, jec !< data/compute domain indices
  integer                       :: isd, ied, jsd, jed 
  character(len=32)             :: strMax, strMin

  call mpp_init(mpp_init_test_init_true_only)
  call mpp_io_init()
  call mpp_domains_init()
  call mpp_set_stack_size(3145746)
  call mpp_domains_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  !> define domains and allocate
  call mpp_define_domains( (/1,length,1,length/), (/4,2/), domain, xhalo=0)
  call mpp_get_compute_domain(domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(domain, isd, ied, jsd, jed)
  allocate(dataI4(isd:ied, jsd:jed),dataI8(isd:ied, jsd:jed), rands(length*length))
  allocate(dataR4(isd:ied, jsd:jed), dataR8(isd:ied, jsd:jed))
  !> make random arrays
  call random_seed()
  call random_number(rands)
  do i=isc, iec-1
    do j=jsc, jec-1
      rcoef = rands(j + i*length) * 2 -1
      dataI4(i, j) = int(rcoef * huge(dataI4), kind=i4_kind)
      dataI8(i, j) = int(rcoef * huge(dataI8), kind=i8_kind)
      dataR4(i, j) = real(rcoef, kind=r4_kind)
      dataR8(i, j) = real(rcoef, kind=r8_kind)
    end do
  end do
  !> test global max and mins from each kind
  call mpp_error(NOTE, "----------Testing 32-bit int mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(dataI4, domain)
  maxI4 = mpp_global_max(domain, dataI4)
  minI4 = mpp_global_min(domain, dataI4)
  write(strMax, *) maxI4
  write(strMin, *) minI4
  if(.NOT. checkResultInt4((/minI4, maxI4 /))) then
    call mpp_error(FATAL, "test_global_minmax: invalid 32-bit integer results"// &
                               NEW_LINE('a')//"Max: "//strMax//" Min: "//strMin )
  endif
  call mpp_sync()

  call mpp_error(NOTE, "----------Testing 64-bit int mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(dataI8, domain)
  maxI8 = mpp_global_max(domain, dataI8)
  minI8 = mpp_global_min(domain, dataI8)
  write(strMax, *) maxI8
  write(strMin, *) minI8
  if(.NOT. checkResultInt8((/minI8, maxI8 /))) then
    call mpp_error(FATAL, "test_global_minmax: invalid 64-bit integer results"// &
                               NEW_LINE('a')//"Max: "//strMax//" Min: "//strMin )
  endif
  call mpp_sync()

  call mpp_error(NOTE, "----------Testing 32-bit real mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(dataR4, domain)
  maxR4 = mpp_global_max(domain, dataR4)
  minR4 = mpp_global_min(domain, dataR4)
  write(strMax, *) maxR4
  write(strMin, *) minR4
  if(.NOT. checkResultReal4((/minR4, maxR4 /))) then
    call mpp_error(FATAL, "test_global_minmax: invalid 32-bit real results"// &
                               NEW_LINE('a')//"Max: "//strMax//" Min: "//strMin )
  endif
  call mpp_sync()

  call mpp_error(NOTE, "----------Testing 64-bit real mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(dataR8, domain)
  maxR8 = mpp_global_max(domain, dataR8)
  minR8 = mpp_global_min(domain, dataR8)
  write(strMax, *) maxR8
  write(strMin, *) minR8
  if(.NOT. checkResultReal8((/minR8, maxR8 /))) then
    call mpp_error(FATAL, "test_global_minmax: invalid 64-bit real results"// &
                               NEW_LINE('a')//"Max: "//strMax//" Min: "//strMin )
  endif

  deallocate(dataI4, dataI8, dataR4, dataR8, rands)
  call mpp_domains_exit()
  call MPI_FINALIZE(ierr)
  
  contains

!> check functions that take min and max for each kind
!> true if all pes return the same result and have a lower/higher local max/min 
function checkResultInt4(res)
  logical                               :: checkResultInt4
  integer(i4_kind),intent(in)           :: res(2)
  integer(i4_kind),allocatable          :: tres(:)
  !> set res to given var and check global max/min with locals
  allocate(tres(2))
  checkResultInt4 = res(2).GE.maxval(dataI4) .and. res(1).LE.minval(dataI4)
  if(.NOT.checkResultInt4) then
    return
  end if
  !> check that all pes have same results
  if( pe.EQ.root) then
    tres = res
    do i=1, npes-1 
      call mpp_send(tres,2, i)
    end do
    checkResultInt4 = .true.
  else
    call mpp_recv(tres,2, root)
    checkResultInt4 = checkResultInt4 .and. res(1) .EQ. tres(1) .and. res(2) .eq. tres(2)
  end if
  deallocate(tres)
end function checkResultInt4 

function checkResultInt8(res)
  logical                               :: checkResultInt8
  integer(i8_kind),intent(in)           :: res(2)
  integer(i8_kind),allocatable          :: tres(:)
  !> set res to given var and check global max/min with locals
  allocate(tres(2))
  checkResultInt8 = res(2).GE.maxval(dataI8) .and. res(1).LE.minval(dataI8)
  if(.NOT.checkResultInt8) then
    return  
  end if
  !> check that all pes have same results
  if( pe.EQ.root) then
    tres = res
    do i=1, npes-1 
      call mpp_send(tres,2, i)
    end do
    checkResultInt8 = .true.
  else
    call mpp_recv(tres,2, root)
    checkResultInt8 = checkResultInt8 .and. res(1) .EQ. tres(1) .and. res(2) .eq. tres(2)
  end if
  deallocate(tres)
end function checkResultInt8

function checkResultReal4(res)
  logical                            :: checkResultReal4
  real(r4_kind),intent(in)           :: res(2)
  real(r4_kind),allocatable          :: tres(:)
  !> set res to given var and check global max/min with locals
  allocate(tres(2))
  checkResultReal4 = res(2).GE.maxval(dataR4) .and. res(1).LE.minval(dataR4)
  if(.NOT. checkResultReal4) then
    return
  end if
  !> check that all pes have same results
  if( pe.EQ.root) then
    tres = res
    do i=1, npes-1 
      call mpp_send(tres,2, i)
    end do
    checkResultReal4 = .true.
  else
    call mpp_recv(tres,2, root)
    checkResultReal4 = checkResultReal4 .and. res(1) .EQ. tres(1) .and. res(2) .eq. tres(2)
  end if
  deallocate(tres)
end function checkResultReal4

function checkResultReal8(res)
  logical                            :: checkResultReal8
  real(r8_kind),intent(in)           :: res(:)
  real(r8_kind),allocatable          :: tres(:)
  !> set res to given var and check global max/min with locals
  allocate(tres(2))
  checkResultReal8 = res(2).GE.maxval(dataR8) .and. res(1).LE.minval(dataR8)
  if(.NOT.checkResultReal8) then
    return
  end if
  !> check that all pes have same results
  if( pe.EQ.root) then
    tres = res
    do i=1, npes-1 
      call mpp_send(tres,2, i)
    end do
    checkResultReal8 = .true.
  else
    call mpp_recv(tres,2, root)
    checkResultReal8 = checkResultReal8 .and. res(1) .EQ. tres(1) .and. res(2) .eq. tres(2)
  end if
  deallocate(tres)
end function checkResultReal8
end program test_global_minmax
