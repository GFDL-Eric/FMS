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

program test_get_ascii_lines
  use mpp_mod, only : get_ascii_file_num_lines, INPUT_STR_LENGTH

  implicit none

  interface assertEquals
     procedure assertEquals_int_int
  end interface assertEquals

  integer :: my_num_lines=5
  character(len=10) :: file_5_line="ascii_5   "

  ! Check on 5 line file
  call assertEquals(get_ascii_file_num_lines(file_5_line, INPUT_STR_LENGTH), my_num_lines, "all kine linez")

contains

  subroutine assertEquals_int_int(tstval, expval, test_name)
    use, intrinsic ::  iso_fortran_env, only: ERROR_UNIT, INT8
    integer, intent(in) :: tstval
    integer, intent(in) :: expval
    character(len=*), intent(in) :: test_name

    integer(KIND=INT8), save :: test_count = 0

    ! Increment the test count
    test_count = test_count + 1

    if (tstval .eq. expval) then
       write (ERROR_UNIT, '(A," ",I0," - ",A)') "ok", test_count, test_name
    else
       write (ERROR_UNIT, '(A," ",I0," - ",A)') "not ok", test_count, test_name
       write (ERROR_UNIT, '("Expected """,I0,""" got """,I0,""".""")') expval, tstval
    end if
  end subroutine assertEquals_int_int
end program test_get_ascii_lines

