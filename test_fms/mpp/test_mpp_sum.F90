
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
!> @file
!! @author Lauren Chilutti
!! @brief Test program for the mpp_sum interface.
!! @email gfdl.climate.model.info@noaa.gov
!! @description This test program is for testing the mpp_sum interface.

program test_mpp_sum
#include <fms_platform.h>

  use mpp_mod, only : mpp_init, mpp_pe, mpp_npes, mpp_root_pe
  use mpp_mod, only : mpp_sync
  use mpp_mod, only : mpp_set_stack_size, mpp_init_test_requests_allocated
  use mpp_mod, only : mpp_sum, mpp_exit
  use mpp_mod, only : mpp_error, FATAL

  implicit none

  integer                                      :: n
  real(FLOAT_KIND), allocatable, dimension(:)  :: a4, b4, c4
  real(DOUBLE_KIND), allocatable, dimension(:) :: a8, b8, c8
  integer                                      :: i, pe, npes, root
  integer, allocatable, dimension(:)           :: pelist

  call mpp_init(mpp_init_test_requests_allocated)
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_sum <------------------'
    call test_mpp_sum_dim1()
    call test_mpp_sum_pelist()
    call test_mpp_sum_large()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_sum <------------------'

  call mpp_exit()

contains

  subroutine test_mpp_sum_dim1()

! Verify that mpp_sum calculates the sum for array with dim=1
  n=1
  allocate( a4(n), a8(n), b4(npes), b8(npes), c4(n), c8(n))

  a4 = real(pe+1, kind=FLOAT_KIND)
  a8 = real(pe+1, kind=DOUBLE_KIND)
  call mpp_sync()
  call mpp_sum(a4(1:n),n)
  call mpp_sum(a8(1:n),n)
  if (pe .EQ. root) then
    b4 = (/(i, i=1,npes, 1)/)
    b8 = (/(i, i=1,npes, 1)/)
    c4 = sum(b4)
    c8 = sum(b8)

    if (a4(1) .ne. c4(1)) call mpp_error(FATAL, "Real4: mpp_sum differs from fortran intrinsic sum")
    if (a8(1) .ne. c8(1)) call mpp_error(FATAL, "Real8: mpp_sum differs from fortran intrinsic sum")
  endif
  deallocate(a4, b4, c4, a8, b8, c8)

  end subroutine test_mpp_sum_dim1

  subroutine test_mpp_sum_pelist()

! Verify mpp_sum accurately sums from select list of pes
  allocate( a4(n), a8(n), b4(npes), b8(npes), c4(n), c8(n))
  allocate( pelist(0:npes-1) )

  a4 = real(pe+1, kind=FLOAT_KIND)
  a8 = real(pe+1, kind=DOUBLE_KIND)
  call mpp_sync()
  if (pe .LE. npes-2) then
    pelist = (/(i,i=0,npes-2)/)
    if (pe .EQ. root) print *,'PE: ',mpp_pe(),' pelist: ',pelist(:)
    call mpp_sum(a4(1:n),n, pelist=pelist)
    call mpp_sum(a8(1:n),n, pelist=pelist)
  endif
  if (pe .EQ. root) then
    b4 = (/(i, i=1,npes-1, 1)/)
    b8 = (/(i, i=1,npes-1, 1)/)
    c4 = sum(b4)
    c8 = sum(b8)
    print *, 'b4: ',b4, 'a4: ',a4, 'c4: ',c4
    if (a4(1) .ne. c4(1)) call mpp_error(FATAL, "Real4 with pelist: mpp_sum differs from fortran intrinsic sum")
    if (a8(1) .ne. c8(1)) call mpp_error(FATAL, "Real8 with pelist: mpp_sum differs from fortran intrinsic sum")
  endif
  deallocate(a4, b4, c4, a8, b8, c8)

  end subroutine test_mpp_sum_pelist

  subroutine test_mpp_sum_large()

! Verify mpp_sum works for large array dim with random values
  n=30000
  allocate( a4(n), a8(n) )

  call random_number(a4)
  call random_number(a8)

  call mpp_sync()
  call mpp_sum(a4(1:n),n)
  call mpp_sum(a8(1:n),n)

! Verify mpp_sum works for large array dim with random values and mpp_sum length less than array dim
  call random_number(a4)
  call random_number(a8)

  call mpp_sync()
  call mpp_sum(a4(1:n),n/2)
  call mpp_sum(a8(1:n),n/2)

  end subroutine test_mpp_sum_large

end program test_mpp_sum
