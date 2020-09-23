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
!! @brief unit test for mpp_alltoall
!! @author MiKyung Lee
!! @description
!! test sending/receiving 1 element so that, for example for npes=4,
!! process0: [ 0, 1, 2, 3]  --alltoall--> [0,10,20,30]
!! process1: [10,11,12,13]  --alltoall--> [1,11,21,31]
!! process2: [20,21,22,23]  --alltoall--> [2,12,22,32]
!! process3: [30,31,32,33]  --alltoall--> [3,13,23,33]
!! and test sending/receiving more than 1 element so that, for example, for npes=4 and Nsend=nrecv=2
!! process0: [ 0, 1, 2, 3, 4, 5, 6, 7]  --alltoall--> [0,1,10,11,20,21,30,31]
!! process1: [10,11,12,13,14,15,16,17]  --alltoall--> [2,3,12,13,22,23,32,33]
!! process2: [20,21,22,23,24,25,26,27]  --alltoall--> [4,5,14,15,24,25,34,35]
!! process3: [30,31,32,33,34,35,36,37]  --alltoall--> [6,7,16,17,26,27,36,37]
!! https://www.olcf.ornl.gov/wp-content/uploads/2018/06/intro_to_HPC_intro_to_mpi.pdf

program test_mpp_alltoall

  use platform_mod
  use mpp_mod, only : mpp_init, mpp_init_test_peset_allocated, mpp_error, FATAL
  use mpp_mod, only : mpp_pe, mpp_npes, mpp_alltoall, mpp_sync

  implicit none

  integer :: npes

    !> initialize MPI
    call mpp_init( test_level=mpp_init_test_peset_allocated )

    !> get total number of pe's
    npes = mpp_npes()

    !> call tests
    call test_mpp_alltoall_real4(npes)
    call test_mpp_alltoall_real8(npes)
    call test_mpp_alltoallv_real4(npes)


contains


  !> test mpp_alltoall for real4
  subroutine test_mpp_alltoall_real4(npes)

    implicit none

    integer, intent(in) :: npes

    integer :: pe, ierr, i, ii, N, isend, jsend, irecv, nsend, nrecv
    integer, allocatable :: pelist(:)
    real(r4_kind), allocatable :: sdata(:), rdata(:)

    !> get pe
    pe = mpp_pe()

    !> test sending/receiving up to npes elements.  can set up to 9 elements.
    nsend = npes
    nrecv = nsend
    if ( npes > 9 ) then
       nsend = 9
       nrecv = 9
    end if

    !> get pelist to pass into mpp_alltoall
    allocate( pelist(0:npes-1) )

    do i=0, npes-1
       pelist(i) = i
    end do

    do isend=1, nsend

       !> allocate sdata (senddata), rdata (receivedata)
       N = isend*npes - 1
       allocate( sdata(0:N), rdata(0:N) )

       !> initialize receiving array
       rdata = real( -1.0, kind=r4_kind )

       !> intialize sending array
       do i=0, N
          sdata(i) = real( 10*pe+i, kind=r4_kind )
       end do

       !> number of elements to send and receive
       irecv = isend

       !> call mpp_alltoall to send/receive one element
       call mpp_alltoall( sdata, isend, rdata, irecv, pelist )

       !> check
       ii = 0
       do i=0, (npes-1)
          do jsend=0, isend-1
             if( rdata(ii) .ne. real( 10*i+isend*pe+jsend, kind=r4_kind ) ) then
                write(*,'("PE #",i3,"element",i4,"Expected",f6.0,"but received",f6.0)') pe, ii, real(10*i+nsend*pe+jsend), rdata(ii)
                call mpp_error(FATAL, 'test_mpp_alltoall failed')
             end if
             ii = ii + 1
          end do
       end do

       deallocate( sdata, rdata )

    end do

  end subroutine test_mpp_alltoall_real4


  !> test mpp_alltoall for real
  subroutine test_mpp_alltoall_real8(npes)

    implicit none

    integer, intent(in) :: npes

    integer :: pe, ierr, i, ii, N, isend, jsend, irecv, nsend, nrecv
    integer, allocatable :: pelist(:)
    real(r8_kind), allocatable :: sdata(:), rdata(:)

    !> get pe
    pe = mpp_pe()

    !> test sending/receiving up to npes elements.  can set up to 9 elements.
    nsend = npes
    nrecv = nsend
    if ( npes > 9 ) then
       nsend = 9
       nrecv = 9
    end if

    !> get pelist to pass into mpp_alltoall
    allocate( pelist(0:npes-1) )
    do i=0, npes-1
       pelist(i) = i
    end do

    do isend=1, nsend

       !> allocate sdata (senddata), rdata (receivedata)
       N = isend*npes - 1
       allocate( sdata(0:N), rdata(0:N) )

       !> initialize receiving array
       rdata = real( -1.0, kind=r8_kind )

       !> intialize sending array
       do i=0, N
          sdata(i) = real( 10*pe+i, kind=r8_kind )
       end do

       !> number of elements to send and receive
       irecv = isend

       !> call mpp_alltoall to send/receive one element
       call mpp_alltoall( sdata, isend, rdata, irecv, pelist )

       !> check
       ii = 0
       do i=0, (npes-1)
          do jsend=0, isend-1
             if( rdata(ii) .ne. real( 10*i+isend*pe+jsend, kind=r8_kind ) ) then
                write(*,'("PE #",i3,"element",i4,"Expected",f6.0,"but received",f6.0)') pe, ii, real(10*i+nsend*pe+jsend), rdata(ii)
                call mpp_error(FATAL, 'test_mpp_alltoall failed')
             end if
             ii = ii + 1
          end do
       end do

       deallocate( sdata, rdata )

    end do

  end subroutine test_mpp_alltoall_real8


  !> test mpp_alltoallv for real4
  subroutine test_mpp_alltoallv_real4(npes)

    implicit none

    integer, intent(in) :: npes

    integer :: pe, ierr, i, ii, N
    integer, allocatable :: pelist(:), nsend(:), nrecv(:), sdispl(:), rdispl(:)
    real(r4_kind), allocatable :: sdata(:), rdata(:), sdata2(:), rdata2(:)

    !> get pe
    pe = mpp_pe()

    N = npes - 1

    allocate( sdata(0:N), rdata(0:N) )
    allocate( nsend(0:N), nrecv(0:N) )
    allocate( sdispl(0:N), rdispl(0:N) )

    !> get pelist to pass into mpp_alltoall
    allocate( pelist(0:N) )
    do i=0, N
       pelist(i) = i
    end do

    !> test sending 1 element and receiving none
    !! process0: [ 0, 1, 2, 3]  --alltoallv--> [-1,-1,-1,-1]
    !! process1: [10,11,12,13]  --alltoallv--> [-1,-1,-1,-1]
    !! process2: [20,21,22,23]  --alltoallv--> [-1,-1,-1,-1]
    !! process3: [30,31,32,33]  --alltoallv--> [-1,-1,-1,-1]

    do i=0, N
       nsend(i)  = 1  !! send one element to each processor
       sdispl(i) = i  !! send ith element to the ith processor
    end do

    !! data to send
    do i=0, N
       sdata(i) = real( 10*pe+i, kind=r4_kind )
    end do

    do i=0, N
       nrecv(i) = 0 !! receive none from each processor
       rdispl(i) = i
    end do

    !! initialize receiving buffer
    rdata = real( -1.0, kind=r4_kind )

    call mpp_alltoall(sdata, nsend, sdispl, rdata, nrecv, rdispl )
    !do i=0, N
    !   if( rdata(i).ne.real(-1.0, kind=r4_kind) ) then
    !      call mpp_error( FATAL, 'test mpp_alltoallv, expected value of -1.0' )
    !   end if
    !end do


    !>send one, receive one
    !! process0: [ 0, 1, 2, 3]  --alltoallv--> [0,10,20,30]
    !! process1: [10,11,12,13]  --alltoallv--> [1,11,21,31]
    !! process2: [20,21,22,23]  --alltoallv--> [2,12,22,32]
    !! process3: [30,31,32,33]  --alltoallv--> [3,13,23,33]

    nsend = 1
    nrecv = 1

    do i=0, N
       sdata(i) = real( 10*pe+i, kind=r4_kind )
    end do

    do i=0, N
       sdispl(i) = i
       rdispl(i) = i
    end do

    rdata = real( -1.0, kind=r4_kind )

    call mpp_alltoall(sdata, nsend, sdispl, rdata, nrecv, rdispl )
    !do i=0, N
    !   if ( rdata(i).ne.real(10*i+pe, kind=r4_kind) ) then
    !      call mpp_error( FATAL, 'test mpp_alltoallv fail' )
    !   end if
    !end do


    !>send one element, receive one
    !! process0: [ 0, 1, 2, 3, 4, 5, 6, 7]  --alltoallv--> [0,-1,10,-1,20,-1,30,-1]
    !! process1: [10,11,12,13,14,15,16,17]  --alltoallv--> [2,-1,12,-1,22,-1,32,-1]
    !! process2: [20,21,22,23,24,25,26,27]  --alltoallv--> [4,-1,14,-1,24,-1,34,-1]
    !! process3: [30,31,32,33,34,35,36,37]  --alltoallv--> [6,-1,16,-1,26,-1,36,-1]

    nsend = 1
    nrecv = 1

    !deallocate( sdata, rdata )
    allocate( sdata2(0:2*npes-1), rdata2(0:2*npes-1) )

    do i=0, N
       sdispl(i) = 2*i
    end do

    do i=0, N
       rdispl(i) = 2*i
    end do

    do i=0, N
       sdata2(2*i)   = real( 10*pe+2*i, kind=r4_kind )
       sdata2(2*i+1) = real( 10*pe+2*i+1, kind=r4_kind )
    end do

    rdata2 = real(-1.0, kind=r4_kind )

    call mpp_alltoall(sdata2, nsend, sdispl, rdata2, nrecv, rdispl )

    write(*,*) pe, ':', rdata2

    !> check
    !do i=1, N
    !   if( rdata(i)


  end subroutine test_mpp_alltoallv_real4

end program test_mpp_alltoall
