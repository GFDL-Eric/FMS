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

  use mpp_mod, only : mpp_init, mpp_pe, mpp_npes, mpp_root_pe, stdout
  use mpp_mod, only : mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync
  use mpp_mod, only : mpp_declare_pelist, mpp_set_current_pelist, mpp_set_stack_size
  use mpp_mod, only : mpp_broadcast, mpp_sum, mpp_min, mpp_max

  implicit none

  integer, parameter              :: n=1048576
  real(FLOAT_KIND), allocatable, dimension(:) :: a
  real(DOUBLE_KIND), allocatable, dimension(:) :: a8
  integer                         :: id, pe, npes, root, i, out_unit, ierr

  call mpp_init(0)
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()
  allocate( a(n), a8(n) )

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_max_r4 <------------------'
    call test_mpp_max_r4()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_max_r4 <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_min_r4 <------------------'
    call test_mpp_min_r4()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_min_r4 <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_max_r8 <------------------'
    call test_mpp_max_r8()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_max_r8 <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_min_r8 <------------------'
    call test_mpp_min_r8()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_min_r8 <------------------'

  deallocate( a, a8 )
  call MPI_FINALIZE(ierr)

contains

  subroutine test_mpp_max_r4

  a = real(pe+1, kind=FLOAT_KIND)
  print *, 'pe,     pe+1 =', pe, a(1)
  call mpp_max( a(1) )
  print *, 'pe, max(pe+1)=', pe, a(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and max using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )call mpp_broadcast( a, n, npes-2, (/(i,i=0,npes-2)/) )
     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a(1)
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )then
        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
        id = mpp_clock_id( 'Partial mpp_sum' )
        call mpp_clock_begin(id)
        call mpp_sum( a(1:1000), 1000, (/(i,i=0,npes-2)/) )
        call mpp_clock_end  (id)
     end if
     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a(1)
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )call mpp_max( a(1), (/(i,i=0,npes-2)/) )
     if( pe.EQ.root )print *, 'max(pe+1) from 0 to npes-2=', a(1)
  end if
  call mpp_set_current_pelist()

  end subroutine test_mpp_max_r4

  subroutine test_mpp_min_r4

  a = real(pe+1, kind=FLOAT_KIND)
  print *, 'pe,     pe+1 =', pe, a(1)
  call mpp_min( a(1) )
  print *, 'pe, min(pe+1)=', pe, a(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and min using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )call mpp_broadcast( a, n, npes-2, (/(i,i=0,npes-2)/) )
     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a(1)
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )then
        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
        id = mpp_clock_id( 'Partial mpp_sum' )
        call mpp_clock_begin(id)
        call mpp_sum( a(1:1000), 1000, (/(i,i=0,npes-2)/) )
        call mpp_clock_end  (id)
     end if
     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a(1)
     a = real(pe+1, kind=FLOAT_KIND)
     if( pe.NE.npes-1 )call mpp_min( a(1), (/(i,i=0,npes-2)/) )
     if( pe.EQ.root )print *, 'min(pe+1) from 0 to npes-2=', a(1)
  end if
  call mpp_set_current_pelist()

  end subroutine test_mpp_min_r4

  subroutine test_mpp_max_r8

  a8 = real(pe+1, kind=DOUBLE_KIND)
!  print *, 'pe,     pe+1 =', pe, a8(1)
  call mpp_max( a8(1) )
  print *, 'pe, max(pe+1)=', pe, a8(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and max using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )call mpp_broadcast( a8, n, npes-2, (/(i,i=0,npes-2)/) )
     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a8(1)
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )then
        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
        id = mpp_clock_id( 'Partial mpp_sum' )
        call mpp_clock_begin(id)
        call mpp_sum( a8(1:1000), 1000, (/(i,i=0,npes-2)/) )
        call mpp_clock_end  (id)
     end if
     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a8(1)
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )call mpp_max( a8(1), (/(i,i=0,npes-2)/) )
     if( pe.EQ.root )print *, 'max(pe+1) from 0 to npes-2=', a8(1)
  end if
  call mpp_set_current_pelist()

  end subroutine test_mpp_max_r8

  subroutine test_mpp_min_r8

  a8 = real(pe+1, kind=DOUBLE_KIND)
  print *, 'pe,     pe+1 =', pe, a8(1)
  call mpp_min( a8(1) )
  print *, 'pe, min(pe+1)=', pe, a8(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and min using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )call mpp_broadcast( a8, n, npes-2, (/(i,i=0,npes-2)/) )
     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a8(1)
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )then
        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
        id = mpp_clock_id( 'Partial mpp_sum' )
        call mpp_clock_begin(id)
        call mpp_sum( a8(1:1000), 1000, (/(i,i=0,npes-2)/) )
        call mpp_clock_end  (id)
     end if
     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a8(1)
     a8 = real(pe+1, kind=DOUBLE_KIND)
     if( pe.NE.npes-1 )call mpp_min( a8(1), (/(i,i=0,npes-2)/) )
     if( pe.EQ.root )print *, 'min(pe+1) from 0 to npes-2=', a8(1)
  end if
  call mpp_set_current_pelist()

  end subroutine test_mpp_min_r8

end program test
