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
program test   !test mpp_min and mpp_max functions for various ints and reals
#include <fms_platform.h>

  use mpp_mod, only : mpp_init, mpp_exit, mpp_pe, mpp_npes, mpp_root_pe, stdout
  use mpp_mod, only : mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync
  use mpp_mod, only : mpp_declare_pelist, mpp_set_current_pelist, mpp_set_stack_size
  use mpp_mod, only : mpp_broadcast, mpp_sum, mpp_min, mpp_max

  implicit none

  integer, parameter              :: n=1048576
  real(FLOAT_KIND), allocatable, dimension(:) :: a4
  real(DOUBLE_KIND), allocatable, dimension(:) :: a8
  integer                         :: id, pe, npes, root, i, out_unit

  interface mpp_max_meta
    procedure mpp_max_meta_0d
  end interface mpp_max_meta

  call mpp_init()
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()
  allocate( a4(n), a8(n) )

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_max_r4 <------------------'
    call test_mpp_max_r4(a4)
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_max_r4 <------------------'

!  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_min_r4 <------------------'
!    call test_mpp_min_r4()
!  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_min_r4 <------------------'
!
!  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_max_r8 <------------------'
!    call test_mpp_max_r8()
!  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_max_r8 <------------------'
!
!  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_min_r8 <------------------'
!    call test_mpp_min_r8()
!  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_min_r8 <------------------'

  deallocate( a4, a8 )
  call mpp_exit()

contains

  subroutine mpp_max_meta_0d(my_polymorphic_object)
  class(real), intent(in) :: my_polymorphic_object
  real(kind=FLOAT_KIND) :: f4_mmax0d
  real(kind=DOUBLE_KIND) :: f8_mmax0d
!  integer(kind=INT_KIND) :: i4_mmax0d
!  integer(kind=LONG_KIND) :: i8_mmax0d
  select type(my_polymorphic_object)
    type is (real(kind=FLOAT_KIND))
      f4_mmax0d = my_polymorphic_object
      call mpp_max(f4_mmax0d)
    type is (real(kind=DOUBLE_KIND))
      f8_mmax0d = my_polymorphic_object
      call mpp_max(f8_mmax0d)
!    type is (integer(kind=INT_KIND))
!      i4_mmax0d = my_polymorphic_object
!      call mpp_max(i4_mmax0d)
!    type is (integer(kind=LONG_KIND))
!      i8_mmax0d = my_polymorphic_object
!      call mpp_max(i8_mmax0d)
  end select 
  end subroutine mpp_max_meta_0d

  function get_test_type(in_data) result(out_data)
  class(real), intent(in), dimension(:) :: in_data
  class(real), allocatable, dimension(:) :: out_data
  real(kind=FLOAT_KIND), dimension(n) :: f4
  real(kind=DOUBLE_KIND), dimension(n) :: f8
!  integer(kind=INT_KIND), dimension(n) :: i4
!  integer(kind=LONG_KIND), dimension(n) :: i8

  select type(in_data)
    type is (real(kind=FLOAT_KIND))
      f4 = real(pe+1, kind=FLOAT_KIND)
      out_data = f4
    type is (real(kind=DOUBLE_KIND))
      f8 = real(pe+1, kind=DOUBLE_KIND)
      out_data = f8
!    type is (integer(kind=INT_KIND))
!      i4 = int(pe+1, kind=INT_KIND)
!      out_data = i4
!    type is (integer(kind=LONG_KIND))
!      i8 = int(pe+1, kind=LONG_KIND)
!      out_data = i8
  end select 

  end function get_test_type

  subroutine test_mpp_max_r4(b)
  class(real), intent(in), dimension(:) :: b
  class(real), allocatable, dimension(:) :: a

  a = get_test_type(b)
  print *, 'pe,     pe+1 =', pe, a(1)
  call mpp_max_meta( a(1) )
  print *, 'pe, max(pe+1)=', pe, a(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and max using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )call mpp_broadcast( a, n, npes-2, (/(i,i=0,npes-2)/) )
!     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a(1)
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )then
!        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
!        id = mpp_clock_id( 'Partial mpp_sum' )
!        call mpp_clock_begin(id)
!        call mpp_sum( a(1:1000), 1000, (/(i,i=0,npes-2)/) )
!        call mpp_clock_end  (id)
!     end if
!     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a(1)
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )call mpp_max( a(1), (/(i,i=0,npes-2)/) )
!     if( pe.EQ.root )print *, 'max(pe+1) from 0 to npes-2=', a(1)
  end if
  call mpp_set_current_pelist()

  end subroutine test_mpp_max_r4

!  subroutine test_mpp_min_r4
!
!  a = real(pe+1, kind=FLOAT_KIND)
!  print *, 'pe,     pe+1 =', pe, a(1)
!  call mpp_min( a(1) )
!  print *, 'pe, min(pe+1)=', pe, a(1)
!  !pelist check
!  call mpp_sync()
!  call flush(out_unit)
!  if( npes.GE.2 )then
!     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and min using PEs 0...npes-2 (excluding last PE)'
!     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )call mpp_broadcast( a, n, npes-2, (/(i,i=0,npes-2)/) )
!     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a(1)
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )then
!        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
!        id = mpp_clock_id( 'Partial mpp_sum' )
!        call mpp_clock_begin(id)
!        call mpp_sum( a(1:1000), 1000, (/(i,i=0,npes-2)/) )
!        call mpp_clock_end  (id)
!     end if
!     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a(1)
!     a = real(pe+1, kind=FLOAT_KIND)
!     if( pe.NE.npes-1 )call mpp_min( a(1), (/(i,i=0,npes-2)/) )
!     if( pe.EQ.root )print *, 'min(pe+1) from 0 to npes-2=', a(1)
!  end if
!  call mpp_set_current_pelist()
!
!  end subroutine test_mpp_min_r4
!
!  subroutine test_mpp_max_r8
!
!  a8 = real(pe+1, kind=DOUBLE_KIND)
!!  print *, 'pe,     pe+1 =', pe, a8(1)
!  call mpp_max( a8(1) )
!  print *, 'pe, max(pe+1)=', pe, a8(1)
!  !pelist check
!  call mpp_sync()
!  call flush(out_unit)
!  if( npes.GE.2 )then
!     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and max using PEs 0...npes-2 (excluding last PE)'
!     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )call mpp_broadcast( a8, n, npes-2, (/(i,i=0,npes-2)/) )
!     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a8(1)
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )then
!        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
!        id = mpp_clock_id( 'Partial mpp_sum' )
!        call mpp_clock_begin(id)
!        call mpp_sum( a8(1:1000), 1000, (/(i,i=0,npes-2)/) )
!        call mpp_clock_end  (id)
!     end if
!     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a8(1)
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )call mpp_max( a8(1), (/(i,i=0,npes-2)/) )
!     if( pe.EQ.root )print *, 'max(pe+1) from 0 to npes-2=', a8(1)
!  end if
!  call mpp_set_current_pelist()
!
!  end subroutine test_mpp_max_r8
!
!  subroutine test_mpp_min_r8
!
!  a8 = real(pe+1, kind=DOUBLE_KIND)
!  print *, 'pe,     pe+1 =', pe, a8(1)
!  call mpp_min( a8(1) )
!  print *, 'pe, min(pe+1)=', pe, a8(1)
!  !pelist check
!  call mpp_sync()
!  call flush(out_unit)
!  if( npes.GE.2 )then
!     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and min using PEs 0...npes-2 (excluding last PE)'
!     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )call mpp_broadcast( a8, n, npes-2, (/(i,i=0,npes-2)/) )
!     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a8(1)
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )then
!        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
!        id = mpp_clock_id( 'Partial mpp_sum' )
!        call mpp_clock_begin(id)
!        call mpp_sum( a8(1:1000), 1000, (/(i,i=0,npes-2)/) )
!        call mpp_clock_end  (id)
!     end if
!     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a8(1)
!     a8 = real(pe+1, kind=DOUBLE_KIND)
!     if( pe.NE.npes-1 )call mpp_min( a8(1), (/(i,i=0,npes-2)/) )
!     if( pe.EQ.root )print *, 'min(pe+1) from 0 to npes-2=', a8(1)
!  end if
!  call mpp_set_current_pelist()
!
!  end subroutine test_mpp_min_r8

end program test
