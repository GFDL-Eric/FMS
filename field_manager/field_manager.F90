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
!> @defgroup field_manager_mod field_manager_mod
!> @ingroup field_manager
!> @brief Reads entries from a field table and stores this
!! information along with the type  of field it belongs to.
!!
!> This allows the component models to query the field manager to see if non-default
!! methods of operation are desired. In essence the field table is a
!! powerful type of namelist. Default values can be provided for all the
!! fields through a namelist, individual fields can be modified  through
!! the field table however.
!!
!> @author William Cooke
!!
!! An example of field table entries could be
!! <PRE>
!!              "tracer","atmos_mod","sphum"
!!
!!              "tracer","atmos_mod","sf6"
!!              "longname","sulf_hex"
!!              "advection_scheme_horiz","2nd_order"
!!              "Profile_type","Fixed","surface_value = 0.0E+00"/
!!
!!              "prog_tracers","ocean_mod","age_global"
!!              horizontal-advection-scheme = mdfl_sweby
!!              vertical-advection-scheme = mdfl_sweby
!!              restart_file = ocean_age.res.nc
!! </PRE>
!!
!! The field table consists of entries in the following format.
!!
!! The first line of an entry should consist of three quoted strings.
!!
!! The first quoted string will tell the field manager what type of
!! field it is.
!!
!! The second quoted string will tell the field manager which model the
!! field is being applied to.
!! The supported types at present are
!!<PRE>
!!      "coupler_mod" for the coupler,
!!      "atmos_mod" for the atmosphere model,
!!      "ocean_mod" for the ocean model,
!!      "land_mod" for the land model, and,
!!      "ice_mod" for the ice model.
!!</PRE>
!! The third quoted string should be a unique name that can be used as a
!! query.
!!
!! The second and following lines of each entry are called methods in
!! this context. Methods can be developed within any module and these
!! modules can query the field manager to find any methods that are
!! supplied in the field table.
!!
!! These lines can be coded quite flexibly.
!!
!! The line can consist of two or three quoted strings or a simple unquoted
!! string.
!!
!! If the line consists two or three quoted strings, then the first string will
!! be an identifier that the querying module will ask for.
!!
!! The second string will be a name that the querying module can use to
!! set up values for the module.
!!
!! The third string, if present, can supply parameters to the calling module that can be
!! parsed and used to further modify values.
!!
!! If the line consists of a simple unquoted string then quotes are not allowed
!! in any part of the line.
!!
!! An entry is ended with a backslash (/) as the final character in a
!! row.
!!
!! Comments can be inserted in the field table by having a # as the
!! first character in the line.
!!
!! In the example above we have three field entries.
!!
!! The first is a simple declaration of a tracer called "sphum".
!!
!! The second is for a tracer called "sf6". In this case a field named
!! "longname" will be given the value "sulf_hex". A field named
!! "advection_scheme_horiz" will be given the value "2nd_order". Finally a field
!! name "Profile_type" will be given a child field called "Fixed", and that field
!! will be given a field called "surface_value" with a real value of 0.0E+00.
!!
!! The third entry is an example of a oceanic age tracer. Note that the
!! method lines are formatted differently here. This is the flexibility mentioned
!! above.
!!
!! With these formats, a number of restrictions are required.
!!
!! The following formats are equally valid.
!!<PRE>
!!      "longname","sulf_hex"
!!      "longname = sulf_hex"
!!      longname = sulf_hex
!!</PRE>
!! However the following is not valid.
!!<PRE>
!!      longname = "sulf_hex"
!!</PRE>
!!
!! In the SF6 example above the last line of the entry could be written in the
!! following ways.
!!<PRE>
!!      "Profile_type","Fixed","surface_value = 0.0E+00"/
!!      Profile_type/Fixed/surface_value = 0.0E+00/
!!</PRE>
!!
!! Values supplied with fields are converted to the various types with the
!! following assumptions.
!!<PRE>
!! Real values : These values contain a decimal point or are in exponential format.
!!    These values only support e or E format for exponentials.
!!    e.g. 10.0, 1e10 and 1E10 are considered to be real numbers.
!!
!! Integer values : These values only contain numbers.
!!    e.g 10 is an integer. 10.0 and 1e10 are not.
!!
!! Logical values : These values are supplied as one of the following formats.
!!    T, .T., TRUE, .TRUE.
!!    t, .t., true, .true.
!!    F, .F., FALSE, .FALSE.
!!    f, .f., false, .false.
!!    These will be converted to T or F in a dump of the field.
!!
!! Character strings : These values are assumed to be strings if a character
!!    other than an e (or E) is in the value. Numbers can be suppled in the value.
!!    If the value does not meet the criteria for a real, integer or logical type,
!!    it is assumed to be a character type.
!!</PRE>
!! The entries within the field table can be designed by the individual
!! authors of code to allow modification of their routines.
!!

!> @file
!> @brief File for @ref field_manager_mod

!> @addtogroup field_manager_mod
!> @{
module field_manager_mod
#ifndef MAXFIELDS_
#define MAXFIELDS_ 250
#endif

#ifndef MAXFIELDMETHODS_
#define MAXFIELDMETHODS_ 250
#endif

!
! <CONTACT EMAIL="William.Cooke@noaa.gov"> William Cooke
! </CONTACT>
!
! <REVIEWER EMAIL="Richard.Slater@noaa.gov"> Richard D. Slater
! </REVIEWER>
!
! <REVIEWER EMAIL="Matthew.Harrison@noaa.gov"> Matthew Harrison
! </REVIEWER>
!
! <REVIEWER EMAIL="John.Dunne@noaa.gov"> John P. Dunne
! </REVIEWER>
!

use yaml_parser_mod
use    mpp_mod, only : mpp_error,   &
                       FATAL,       &
                       NOTE,        &
                       WARNING,     &
                       mpp_pe,      &
                       mpp_root_pe, &
                       stdlog,      &
                       stdout
use    fms_mod, only : lowercase,   &
                       write_version_number
use fms2_io_mod, only: file_exists

implicit none
private

logical            :: module_is_initialized  = .false.

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Public routines
!        Interface definitions (optional arguments are in [brackets]):
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
public :: field_manager_init   !< (nfields, [table_name]) returns number of fields
public :: field_manager_end    !< ()
public :: get_field_info       !< (n,fld_type,fld_name,model,num_methods)
                               !! Returns parameters relating to field n.
public :: get_field_method     !< (n, m, method) Returns the m-th method of field n
public :: get_field_methods    !< (n, methods) Returns the methods related to field n
public :: parse                !< (text, label, values) Overloaded function to parse integer,
                               !! real or character. Parse returns the number of values
                               !! decoded (> 1 => an array of values)
public :: fm_change_list       !< (list) return success
public :: fm_change_root       !< (list) return success
public :: fm_dump_list         !< (list [, recursive]) return success
public :: fm_exists            !< (field) return success
public :: fm_get_index         !< (field) return index
public :: fm_get_current_list  !< () return path
public :: fm_get_length        !< (list) return length
public :: fm_get_type          !< (field) return string
public :: fm_get_value         !< (entry, value [, index]) return success !! generic
public :: fm_get_value_integer !<   as above (overloaded function)
public :: fm_get_value_logical !<   as above (overloaded function)
public :: fm_get_value_real    !<   as above (overloaded function)
public :: fm_get_value_string  !<   as above (overloaded function)
public :: fm_intersection      !< (lists, num_lists) return fm_array_list pointer
public :: fm_init_loop         !< (list, iter)
public :: fm_loop_over_list    !< (list, name, type, index) return success
                               !! (iter, name, type, index) return success
public :: fm_new_list          !< (list [, create] [, keep]) return index
public :: fm_new_value         !< (entry, value [, create] [, index]) return index !! generic
public :: fm_new_value_integer !<   as above (overloaded function)
public :: fm_new_value_logical !<   as above (overloaded function)
public :: fm_new_value_real    !<   as above (overloaded function)
public :: fm_new_value_string  !<   as above (overloaded function)
public :: fm_reset_loop        !< ()
public :: fm_return_root       !< () return success
public :: fm_modify_name       !< (oldname, newname) return success
public :: fm_query_method      !< (name, method_name, method_control) return success and
                               !! name and control strings
public :: fm_find_methods      !< (list, methods, control) return success and name and
                               !! control strings.
public :: fm_copy_list         !< (list, suffix, [create]) return index

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!   Private routines
!   Interface definitions (optional arguments are in [brackets]):
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

private :: create_field        ! (list_p, name) return field pointer
private :: dump_list           ! (list_p, recursive, depth) return success
private :: find_base           ! (field, path, base)
private :: find_field          ! (field, list_p) return field pointer
private :: find_head           ! (field, head, rest)
private :: find_list           ! (list, list_p, create) return field pointer
private :: get_field           ! (field, list_p) return field pointer
private :: reset_field_tree    ! ()
private :: make_list           ! (list_p, name) return field pointer

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Public parameters
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!> The length of a character string representing the field name.
integer, parameter, public :: fm_field_name_len = 48
!> The length of a character string representing the field path.
integer, parameter, public :: fm_path_name_len  = 512
!> The length of a character string representing character values for the field.
integer, parameter, public :: fm_string_len     = 1024
!> The length of a character string representing the various types that the values of the field can take.
integer, parameter, public :: fm_type_name_len  = 8
!> Number of models (ATMOS, OCEAN, LAND, ICE, COUPLER).
integer, parameter, public :: NUM_MODELS        = 5
!> The value returned if a field is not defined.
integer, parameter, public :: NO_FIELD          = -1
!> Atmospheric model.
integer, parameter, public :: MODEL_ATMOS       = 1
!> Ocean model.
integer, parameter, public :: MODEL_OCEAN       = 2
!> Land model.
integer, parameter, public :: MODEL_LAND        = 3
!> Ice model.
integer, parameter, public :: MODEL_ICE         = 4
!> Ice model.
integer, parameter, public :: MODEL_COUPLER     = 5
!> Model names, e.g. MODEL_NAMES(MODEL_OCEAN) is 'oceanic'
character(len=11), parameter, public, dimension(NUM_MODELS) :: &
   MODEL_NAMES=(/'atmospheric','oceanic    ','land       ','ice        ','coupler    '/)

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Public type definitions
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

!> @}

!> @brief List of field names
!> @ingroup field_manager_mod
type, public :: fm_array_list_def
  character (len=fm_field_name_len), dimension(:), pointer :: names => NULL()
  integer                                                  :: length
end type  fm_array_list_def

!> @brief This method_type is a way to allow a component module to alter the parameters it needs
!! for various tracers.
!!
!> In essence this is a way to modify a namelist. A namelist can supply
!! default parameters for all tracers. This  method will allow the user to modify these
!! default parameters for an individual tracer. An example could be that  the user wishes to
!! use second order advection on a tracer and also use fourth order advection on a second
!! tracer  within the same model run. The default advection could be second order and the
!! field table would then indicate  that the second tracer requires fourth order advection.
!! This would be parsed by the advection routine.
!> @ingroup field_manager_mod
type, public :: method_type

  character(len=fm_string_len) :: method_type !< This string represents a tag that a module
                                 !! using this method can key on. Typically this should
                                 !! contain some reference to the module that is calling it.
  character(len=fm_string_len) :: method_name !< This is the name of a method which the module
                                 !! can parse and use to assign different default values to
                                 !! a field method.
  character(len=fm_string_len) :: method_control !< This is the string containing parameters that
                                 !! the module can use as values  for a field method. These should
                                 !! override default values within the module.
end type

!>   This method_type is the same as method_type except that the
!!   method_control string is not present. This is used when you wish to
!!   change to a scheme within a module but do not need to pass
!!   parameters. See @ref method_type for member information.
!> @ingroup field_manager_mod
type, public :: method_type_short
  character(len=fm_string_len) :: method_type
  character(len=fm_string_len) :: method_name
end type

!>   This is the same as method_type except that the
!!   method_control and method_name strings are not present. This is used
!!   when you wish to change to a scheme within a module but do not need
!!   to pass parameters.
!> @ingroup field_manager_mod
type, public :: method_type_very_short
  character(len=fm_string_len) :: method_type
end type

!> Iterator over the field manager list
!> @ingroup field_manager_mod
type, public :: fm_list_iter_type
   type(field_def), pointer    :: ptr => NULL()  !< pointer to the current field
end type fm_list_iter_type


!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Public types
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

!> @ingroup field_manager_mod
type(method_type), public :: default_method

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Interface definitions for overloaded routines
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

!> @brief A function to parse an integer or an array of integers,
!! a real or an array of reals, a string or an array of strings.
!!
!> Parse is an integer function that decodes values from a text string.
!! The text string has the form: "label=list" where "label" is an
!! arbitrary user defined label describing the values being decoded,
!! and "list" is a list of one or more values separated by commas.
!! The values may be integer, real, or character.
!! Parse returns the number of values decoded.
!! <br>Example usage:
!! @code{.F90}
!! number = parse(text, label, value)
!! @endcode
!> @ingroup field_manager_mod
interface parse
  module procedure  parse_real
  module procedure  parse_reals
  module procedure  parse_integer
  module procedure  parse_integers
  module procedure  parse_string
  module procedure  parse_strings
end interface

!> @brief An overloaded function to assign a value to a field.
!!
!> Allocate and initialize a new value and return the index.
!! If an error condition occurs the parameter NO_FIELD is returned.
!!
!! If the type of the field is changing (e.g. real values being transformed to
!! integers), then any previous values for the field are removed and replaced
!! by the value passed in the present call to this function.
!!
!! If append is present and .true., then index cannot be greater than 0 if
!! it is present.
!! <br> Example usage:
!! @code{.F90}
!! field_index= fm_new_value(name, value, [create], [index], [append])
!! @endcode
!> @ingroup field_manager_mod
interface  fm_new_value
  module procedure  fm_new_value_integer
  module procedure  fm_new_value_logical
  module procedure  fm_new_value_real
  module procedure  fm_new_value_string
end interface

!> @brief An overloaded function to find and extract a value for a named field.
!!
!> Find and extract the value for name. The value may be of type real,
!! integer, logical or character. If a single value from an array  of values
!! is required, an optional index can be supplied.
!! Return true for success and false for failure
!! <br> Example usage:
!! @code{.F90}
!! success = fm_get_value(name, value, index)
!! @endcode
!> @ingroup field_manager_mod
interface  fm_get_value
  module procedure  fm_get_value_integer
  module procedure  fm_get_value_logical
  module procedure  fm_get_value_real
  module procedure  fm_get_value_string
end interface

!> @brief A function for looping over a list.
!!
!> Loop over the list, setting the name, type and index
!! of the next field. Return false at the end of the loop.
!! <br> Example usage:
!! @code{.F90}
!! success = fm_loop_over_list(list, name, field_type, index)
!! @endcode
!> @ingroup field_manager_mod
interface fm_loop_over_list
  module procedure  fm_loop_over_list_new
  module procedure  fm_loop_over_list_old
end interface

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Private parameters
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

character(len=17), parameter :: module_name       = 'field_manager_mod'
character(len=1),  parameter :: comma             = ","
character(len=1),  parameter :: list_sep          = '/'
character(len=33), parameter :: error_header      = '==>Error from '//trim(module_name)//': '
character(len=35), parameter :: warn_header       = '==>Warning from '//trim(module_name)//': '
character(len=32), parameter :: note_header       = '==>Note from '//trim(module_name)//': '

integer,           parameter :: null_type         = 0
integer,           parameter :: integer_type      = 1
integer,           parameter :: list_type         = 2
integer,           parameter :: logical_type      = 3
integer,           parameter :: real_type         = 4
integer,           parameter :: string_type       = 5
integer,           parameter :: num_types         = 5
integer,           parameter :: line_len          = 256
integer,           parameter :: array_increment   = 10
integer,           parameter :: MAX_FIELDS        = MAXFIELDS_
integer,           parameter :: MAX_FIELD_METHODS = MAXFIELDMETHODS_


!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Private type definitions
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

!> @brief Private type for internal use
!> @ingroup field_manager_mod
type, private :: field_mgr_type
  character(len=fm_field_name_len)                    :: field_type
  character(len=fm_string_len)                    :: field_name
  integer                                             :: model, num_methods
  type(method_type)                                   :: methods(MAX_FIELD_METHODS)
end type field_mgr_type

!> @brief Private type for internal use
!> @ingroup field_manager_mod
type, private :: field_names_type
  character(len=fm_field_name_len)                    :: fld_type
  character(len=fm_field_name_len)                    :: mod_name
  character(len=fm_string_len)                    :: fld_name
end  type field_names_type

!> @brief Private type for internal use
!> @ingroup field_manager_mod
type, private :: field_names_type_short
  character(len=fm_field_name_len)                    :: fld_type
  character(len=fm_field_name_len)                    :: mod_name
end type field_names_type_short

!> @brief Private type for internal use
!> @ingroup field_manager_mod
type, private :: field_def
  character (len=fm_field_name_len)                   :: name
  integer                                             :: index
  type (field_def), pointer                           :: parent => NULL()
  integer                                             :: field_type
  integer                                             :: length
  integer                                             :: array_dim
  integer                                             :: max_index
  type (field_def), pointer                           :: first_field => NULL()
  type (field_def), pointer                           :: last_field => NULL()
  integer, pointer, dimension(:)                      :: i_value => NULL()
  logical, pointer, dimension(:)                      :: l_value => NULL()
  real, pointer, dimension(:)                         :: r_value => NULL()
  character(len=fm_string_len), pointer, dimension(:) :: s_value => NULL()
  type (field_def), pointer                           :: next => NULL()
  type (field_def), pointer                           :: prev => NULL()
end type field_def

!> @addtogroup field_manager_mod
!> @{

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Private types
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

#ifdef use_yaml
type(field_mgr_type), dimension(:), allocatable, private :: fields
#else
type(field_mgr_type), private :: fields(MAX_FIELDS)
#endif

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!        Private variables
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

character(len=fm_path_name_len)  :: loop_list
character(len=fm_type_name_len)  :: field_type_name(num_types)
character(len=fm_field_name_len) :: save_root_name
integer                          :: num_fields         = 0
type (field_def), pointer        :: loop_list_p        => NULL()
type (field_def), pointer        :: current_list_p     => NULL()
type (field_def), pointer        :: root_p             => NULL()
type (field_def), pointer        :: save_root_parent_p => NULL()
type (field_def), target, save   :: root

contains

!> @brief Routine to initialize the field manager.
!!
!> This routine reads the fields from a yaml file containing key value pairs.
!! These potentially nested key value pairs contain information on which schemes are
!! needed within various modules. The field manager does not
!! initialize any of those schemes however. It simply holds the
!! information and is queried by the appropriate  module.
subroutine field_manager_init(nfields)

integer, intent(out), optional :: nfields

if (module_is_initialized) then
   nfields = num_fields
   return
endif

call reset_field_tree
call get_fields_from_yaml
num_fields = get_num_fields()
nfields = num_fields
module_is_initialized = .true.

end subroutine field_manager_init

!> @brief Destructor for field manager.
!!
!> This subroutine changes the initialized flag to false.
subroutine field_manager_end

module_is_initialized = .false.

end subroutine field_manager_end

!> @brief A routine to strip whitespace from the start of character strings.
!!
!> This subroutine removes spaces from the start of a character string.
subroutine strip_front_blanks(text)

character(len=*), intent(in) :: text !< name to remove whitespace from

text = trim(adjustl(text))
end subroutine strip_front_blanks

!> @brief This routine allows access to field information given an index.
!!
!> When passed an index, this routine will return the type of field,
!! the name of the field, the model which the field is associated and
!! the number of methods associated with the field.
!! <br>Example usage:
!! @code{.F90}
!! call get_field_info( n,fld_type,fld_name,model,num_methods )
!! @endcode
subroutine get_field_info(n,fld_type,fld_name,model,num_methods)
integer,          intent(in)  :: n !< index of field
character (len=*),intent(out) :: fld_type !< field type
character (len=*),intent(out) :: fld_name !< name of the field
integer, intent(out) :: model !< number indicating which model is used
integer, intent(out) :: num_methods !< number of methods

if (n < 1 .or. n > num_fields) call mpp_error(FATAL,trim(error_header)//'Invalid field index')

fld_type    = fields(n)%field_type
fld_name    = fields(n)%field_name
model       = fields(n)%model
num_methods = fields(n)%num_methods

end subroutine get_field_info

!> @brief A routine to get a specified method
!!
!> This routine, when passed a field index and a method index will
!! return the method text associated with the field(n) method(m).
subroutine get_field_method(n,m,method)

integer,           intent(in)    :: n !< index of field
integer,           intent(in)    :: m !< index of method
type(method_type) ,intent(inout) :: method !< the m-th method of field with index n

if (n < 1 .or. n > num_fields) call mpp_error(FATAL,trim(error_header)//'Invalid field index')

if (m < 1 .or. m > fields(n)%num_methods) call mpp_error(FATAL,trim(error_header)//'Invalid method index')

  method = fields(n)%methods(m)

end subroutine get_field_method

!> @brief A routine to obtain all the methods associated with a field.
!!
!> When passed a field index, this routine will return the text
!! associated with all the methods attached to the field.
subroutine get_field_methods(n,methods)

integer,          intent(in)  :: n !< field index
type(method_type),intent(inout) :: methods(:) !< an array of methods for field with index n

character(len=fm_path_name_len), dimension(size(methods(:))) :: control
character(len=fm_path_name_len), dimension(size(methods(:))) :: method
logical                                                      :: found_methods

  if (n < 1 .or. n > num_fields) &
    call mpp_error(FATAL,trim(error_header)//'Invalid field index')

  if (size(methods(:)) <  fields(n)%num_methods) &
    call mpp_error(FATAL,trim(error_header)//'Method array too small')

  methods = default_method
  methods(1:fields(n)%num_methods) = fields(n)%methods(1:fields(n)%num_methods)

end subroutine get_field_methods

!> @returns The number of values that have been decoded. This allows
!! a user to define a large array and fill it partially with
!! values from a list. This should be the size of the value array.
function parse_reals ( text, label, values ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
real,             intent(out) :: values(:) !< The value or values that have been decoded.

include 'parse.inc'
end function parse_reals

function parse_integers ( text, label, values ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
integer,          intent(out) :: values(:) !< The value or values that have been decoded.

include 'parse.inc'
end function parse_integers

function parse_strings ( text, label, values ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
character(len=*), intent(out) :: values(:) !< The value or values that have been decoded.

include 'parse.inc'
end function parse_strings

!---- scalar overloads -----

function parse_real ( text, label, value ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
real,             intent(out) :: value !< The value or values that have been decoded.
integer :: parse

real :: values(1)

   parse = parse_reals ( text, label, values )
   if (parse > 0) value = values(1)
end function parse_real

function parse_integer ( text, label, value ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
integer,          intent(out) :: value !< The value or values that have been decoded.
integer :: parse

integer :: values(1)

   parse = parse_integers ( text, label, values )
   if (parse > 0) value = values(1)
end function parse_integer

function parse_string ( text, label, value ) result (parse)
character(len=*), intent(in)  :: text !< The text string from which the values will be parsed.
character(len=*), intent(in)  :: label !< A label which describes the values being decoded.
character(len=*), intent(out) :: value !< The value or values that have been decoded.
integer :: parse

character(len=len(value)) :: values(1)

   parse = parse_strings ( text, label, values )
   if (parse > 0) value = values(1)
end function parse_string

!> @brief A function to create a field as a child of parent_p. This will return
!! a pointer to a field_def type.
!!
!> Allocate and initialize a new field in parent_p list.
!! Return a pointer to the field on success, or throw an error
!! on failure.
!! <br>Example usage:
!! @code{.F90}
!! list_p => create_field(parent_p, name)
!! @endcode
function  create_field(parent_p, name)                        &
          result (list_p)
type (field_def), pointer    :: list_p
type (field_def), pointer    :: parent_p !< A pointer to the parent of the field that is to be created
character(len=*), intent(in) :: name !< The name of the field that is to be created

integer                      :: ier
integer                      :: error, out_unit

allocate(list_p, stat = error)
if (error .ne. 0) call mpp_error(FATAL,trim(error_header)//'Unable to allocate space for new list')

list_p%name = name

nullify(list_p%next)
list_p%prev => parent_p%last_field
nullify(list_p%first_field)
nullify(list_p%last_field)
list_p%length = 0
list_p%field_type = null_type
list_p%max_index = 0
list_p%array_dim = 0
if (associated(list_p%i_value)) deallocate(list_p%i_value)
if (associated(list_p%l_value)) deallocate(list_p%l_value)
if (associated(list_p%r_value)) deallocate(list_p%r_value)
if (associated(list_p%s_value)) deallocate(list_p%s_value)

if (parent_p%length .le. 0) then
  parent_p%first_field => list_p
else
  parent_p%last_field%next => list_p
endif

parent_p%last_field => list_p
parent_p%length = parent_p%length + 1
list_p%index = parent_p%length
list_p%parent => parent_p

end function  create_field

!> @brief This is a function that lists the parameters of a field.
!!
!> Given a pointer to a list, this function prints out the fields, and
!! subfields, if recursive is true, associated with the list.
!!
!! This is most likely to be used through fm_dump_list.
!! <br> Example usage:
!! @code{.F90}
!! success = dump_list(list_p, recursive=.true., depth=0)
!! @endcode
logical recursive function dump_list(list_p, recursive, depth, out_unit) result(success)

  type (field_def), pointer :: list_p !< pointer to the field to be printed out
  logical, intent(in)       :: recursive !< flag to make function recursively print subfields
  integer, intent(in)       :: depth !< Listing will be padded so that 'depth' spaces appear before
                                     !! the field being printed
  integer, intent(in)       :: out_unit !< unit number to print to

  integer                             :: depthp1
  integer                             :: j
  character(len=fm_field_name_len)    :: num, scratch
  type (field_def), pointer           :: this_field_p
  character(len=depth+fm_field_name_len) :: blank

  blank = ' ' ! initialize blank string

  ! Check for a valid list
  success = .false.
  if (.not. associated(list_p)) then
    return
  elseif (list_p%field_type .ne. list_type) then
    return
  endif

  ! set the default return value
  success = .true.

  ! Print the name of this list
  write (out_unit,'(a,a,a)') blank(1:depth), trim(list_p%name), list_sep

  !  Increment the indentation depth
  ! The following max function is to work around an error in the IBM compiler for len_trim
  ! depthp1 = depth + max(len_trim(list_p%name),0) + len_trim(list_sep)
  depthp1 = depth + 6

  this_field_p => list_p%first_field

  do while (associated(this_field_p))

     select case(this_field_p%field_type)
     case(list_type)
       ! If this is a list, then call dump_list
       if (recursive) then
          ! If recursive is true, then this routine will find and dump sub-fields.
          success =  dump_list(this_field_p, .true., depthp1, out_unit)
          if (.not.success) exit ! quit immediately in case of error
       else ! Otherwise it will print out the name of this field.
          write (out_unit,'(a,a,a)') blank(1:depthp1), trim(this_field_p%name), list_sep
       endif

     case(integer_type)
         if (this_field_p%max_index .eq. 0) then
            write (out_unit,'(a,a,a)') blank(1:depthp1),  trim(this_field_p%name), ' = NULL'
         elseif (this_field_p%max_index .eq. 1) then
            write (scratch,*) this_field_p%i_value(1)
            write (out_unit,'(a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), ' = ', &
                   trim(adjustl(scratch))
         else  ! Write out the array of values for this field.
            do j = 1, this_field_p%max_index
               write (scratch,*) this_field_p%i_value(j)
               write (num,*) j
               write (out_unit,'(a,a,a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), &
                      '[', trim(adjustl(num)), '] = ', trim(adjustl(scratch))
           enddo
         endif

     case(logical_type)
         if (this_field_p%max_index .eq. 0) then
            write (out_unit,'(a,a,a)') blank(1:depthp1),  trim(this_field_p%name), ' = NULL'
         elseif (this_field_p%max_index .eq. 1) then
            write (scratch,'(l1)') this_field_p%l_value(1)
            write (out_unit,'(a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), ' = ', &
                   trim(adjustl(scratch))
         else  ! Write out the array of values for this field.
            do j = 1, this_field_p%max_index
               write (scratch,'(l1)') this_field_p%l_value(j)
               write (num,*) j
               write (out_unit,'(a,a,a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), &
                      '[', trim(adjustl(num)), '] = ', trim(adjustl(scratch))
            enddo
         endif

     case(real_type)
         if (this_field_p%max_index .eq. 0) then
            write (out_unit,'(a,a,a)') blank(1:depthp1),  trim(this_field_p%name), ' = NULL'
         elseif (this_field_p%max_index .eq. 1) then
            write (scratch,*) this_field_p%r_value(1)
            write (out_unit,'(a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), ' = ', &
                   trim(adjustl(scratch))
         else  ! Write out the array of values for this field.
            do j = 1, this_field_p%max_index
               write (scratch,*) this_field_p%r_value(j)
               write (num,*) j
               write (out_unit,'(a,a,a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), &
                      '[', trim(adjustl(num)), '] = ', trim(adjustl(scratch))
            enddo
         endif

     case(string_type)
         if (this_field_p%max_index .eq. 0) then
            write (out_unit,'(a,a,a)') blank(1:depthp1),  trim(this_field_p%name), ' = NULL'
         elseif (this_field_p%max_index .eq. 1) then
            write (out_unit,'(a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), ' = ', &
                   ''''//trim(this_field_p%s_value(1))//''''
         else  ! Write out the array of values for this field.
            do j = 1, this_field_p%max_index
               write (num,*) j
               write (out_unit,'(a,a,a,a,a,a)') blank(1:depthp1), trim(this_field_p%name), &
                      '[', trim(adjustl(num)), '] = ', ''''//trim(this_field_p%s_value(j))//''''
            enddo
         endif

     case default
         success = .false.
         exit

     end select

     this_field_p => this_field_p%next
  enddo

end function dump_list

!#######################################################################
!#######################################################################

!> @brief A subroutine that splits a listname into a path and a base.
!!
!> Find the base name for a list by splitting the list name into
!! a path and base. The base is the last field within name, while the
!! path is the preceding section of name. The base string can then be
!! used to query for values associated with name.
subroutine find_base(name, path, base)

character(len=*), intent(in)  :: name !< list name for a field
character(len=*), intent(out) :: path !< path of the base field
character(len=*), intent(out) :: base !< A string which can be used to query for values associated with name

integer :: i
integer :: length

if (length .eq. 0) then
   path = ' '
   base = ' '
else
   do while (name(length:length) .eq. list_sep)
      length = length - 1
      if (length .eq. 0) then
         exit
      endif
   enddo
   if (length .eq. 0) then
      path = ' '
      base = ' '
   else
      i = index(name(1:length), list_sep, back = .true.)
      if (i .eq. 0) then
         path = ' '
         base = name(1:length)
      else
         path = name(1:i)
         base = name(i+1:length)
      endif
   endif
endif

end subroutine find_base

!> @brief Find and return a pointer to the field in the specified
!! list. Return a null pointer on error.
!!
!> Find and return a pointer to the field in the specified
!! list. Return a null pointer on error. Given a pointer to a field,
!! this function searchs for "name" as a sub field.
!> @returns A pointer to the field corresponding to "name" or an unassociated pointer if the field
!! name does not exist.
function find_field(name, this_list_p)                                &
        result (field_p)
type (field_def), pointer    :: field_p
character(len=*), intent(in) :: name !< The name of a field that the user wishes to find
type (field_def), pointer    :: this_list_p !< A pointer to a list which the user wishes to search
                                            !! for a field "name".
type (field_def), pointer, save    :: temp_p

nullify (field_p)

if (name .eq. '.') then
  field_p => this_list_p
elseif (name .eq. '..') then
  field_p => this_list_p%parent
else
  temp_p => this_list_p%first_field
  do while (associated(temp_p))
    if (temp_p%name .eq. name) then
      field_p => temp_p
      exit
    endif
    temp_p => temp_p%next
  enddo
endif

end function find_field

!> @brief Find the first list for a name by splitting the name into
!!    a head and the rest.
!!
!> Find the first list for a name by splitting the name into a head and the
!! rest. The head is the first field within name, while rest is the remaining
!! section of name. The head string can then be used to find other fields that
!! may be associated with name.
subroutine find_head(name, head, rest)

character(len=*), intent(in)  :: name !< The name of a field of interest
character(len=*), intent(out) :: head !< the first field within name
character(len=*), intent(out) :: rest !< the remaining section of name

integer        :: i
i = index(name, list_sep)
do while (i .le. len(name))
  if (name(i+1:i+1) .eq. list_sep) then
    i = i + 1
  else
    exit
  endif
enddo

if (i .eq. 0) then
  head = ' '
  rest = name
elseif (i .eq. len(name)) then
  head = name
  rest = ' '
else
  head = name(1:i)
  rest = name(i+1:)
endif

end subroutine find_head

!> @brief Find and return a pointer to the specified list, relative to
!!    relative_p. Return a null pointer on error.
!!
!> This function, when supplied a pointer to a field and a name of a second
!! field relative to that pointer, will find a list and return the pointer to
!! the second field. If create is .true. and the second field does not exist,
!! it will be created.
!> @returns A pointer to the list to be returned
function find_list(path, relative_p, create)                    &
        result (list_p)
type (field_def), pointer        :: list_p
character(len=*), intent(in)     :: path !< path to the list of interest
type (field_def), pointer        :: relative_p !< pointer to the list to which "path" is relative to
logical,          intent(in)     :: create !< If the list does not exist, it will be created if set to true

character(len=fm_path_name_len)  :: working_path
character(len=fm_path_name_len)  :: rest
character(len=fm_field_name_len) :: this_list
integer                          :: i, out_unit
type (field_def), pointer, save  :: working_path_p
type (field_def), pointer, save  :: this_list_p

nullify(list_p)
if (path .eq. ' ') then
  list_p => relative_p
else
  if (path(1:1) .eq. list_sep) then
    working_path_p => root_p
    working_path = path(2:)
  else
    working_path_p => relative_p
    working_path = path
  endif
  do while (working_path .ne. ' ')
    call find_head(working_path, this_list, rest)
    if (this_list .eq. ' ') then
      this_list = rest
      rest = ' '
    endif
    i = len_trim(this_list)
    do while (i .gt. 0 .and. this_list(i:i) .eq. list_sep)
      this_list(i:i) = ' '
      i = i - 1
    enddo
    this_list_p => find_field(this_list, working_path_p)
    if (.not. associated(this_list_p)) then
      if (create) then
        this_list_p => make_list(working_path_p, this_list)
        if (.not. associated(this_list_p)) then
          nullify(list_p)
          return
        endif
      else
        nullify(list_p)
        return
      endif
    endif
    if (this_list_p%field_type .eq. list_type) then
      working_path_p => this_list_p
      working_path = rest
    else
      nullify(list_p)
      return
    endif
  enddo
  list_p => working_path_p
endif

end function find_list

!> @brief Change the current list. Return true on success, false otherwise
!!
!> This function changes the current list to correspond to the list named name.
!! If the first character of name is the list separator (/) then the list will
!! search for "name" starting from the root of the field tree. Otherwise it
!! will search for name starting from the current list.
!! @return A flag to indicate operation success, true = no errors
function fm_change_list(name)                                        &
        result (success)
logical        :: success
character(len=*), intent(in)  :: name !< name of a list to change to

type (field_def), pointer, save :: temp_p

if (.not. module_is_initialized) call field_manager_init
temp_p => find_list(name, current_list_p, .false.)
if (associated(temp_p)) then
  current_list_p => temp_p
  success = .true.
else
  success = .false.
endif

end function fm_change_list

!> @brief Change the root list
!!
!> This function changes the root of the field tree to correspond to the
!! field named name. An example of a use of this would be if code is
!! interested in a subset of fields with a common base. This common base
!! could be set using fm_change_root and fields could be referenced using
!! this root.
!!
!! This function should be used in conjunction with fm_return_root.
!! @return A flag to indicate operation success, true = no errors
function  fm_change_root(name)                                        &
          result (success)
logical        :: success
character(len=*), intent(in)  :: name !< name of the field which the user wishes to become the root.

type (field_def), pointer, save :: temp_list_p

if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') then
  success = .false.
  return
endif
temp_list_p => find_list(name, current_list_p, .false.)
if (associated(temp_list_p)) then
  if (save_root_name .ne. ' ') then
    root_p%name = save_root_name
    root_p%parent => save_root_parent_p
  endif
  root_p => temp_list_p
  save_root_name = root_p%name
  save_root_parent_p => root_p%parent
  root_p%name = ' '
  nullify(root_p%parent)
  current_list_p => root_p
  success = .true.
else
  success = .false.
endif

end function  fm_change_root

!> @brief A function to list properties associated with a field.
!!
!> This function writes the contents of the field named "name" to stdout.
!! If recursive is present and .true., then this function writes out the
!! contents of any subfields associated with the field named "name".
!! @return A flag to indicate operation success, true = no errors
logical function  fm_dump_list(name, recursive, unit) result (success)
  character(len=*), intent(in)  :: name !< The name of the field for which output is requested.
  logical, intent(in), optional :: recursive !< If present and .true., then a recursive listing of
                                             !! fields will be performed.
  integer, intent(in), optional :: unit !< file to print to

  logical                         :: recursive_t
  type (field_def), pointer, save :: temp_list_p
  integer                         :: out_unit

  if (present(unit)) then
     out_unit = unit
  else
     out_unit = stdout()
  endif

  recursive_t = .false.
  if (present(recursive)) recursive_t = recursive
  if (.not. module_is_initialized) call field_manager_init

  if (name .eq. ' ') then
    temp_list_p => current_list_p
    success = .true.
  else
    temp_list_p => find_list(name, current_list_p, .false.)
    if (associated(temp_list_p)) then
       success = .true.
    else
       success = .false.
    endif
  endif
  if (success) then success = dump_list(temp_list_p, recursive_t, 0, out_unit)
end function  fm_dump_list

!> @brief A function to test whether a named field exists.
!!
!> This function determines if a field exists, relative to the current list,
!! and returns true if the list exists, false otherwise.
!! @return A flag to indicate operation success, true = no errors
function fm_exists(name)                                                &
        result (success)
logical        :: success
character(len=*), intent(in) :: name !< The name of the field that is being queried

type (field_def), pointer, save :: dummy_p

if (.not. module_is_initialized) call field_manager_init
dummy_p => get_field(name, current_list_p)
success = associated(dummy_p)

end function fm_exists

!> @brief A function to return the index of a named field.
!!
!> Returns the index for name, returns the parameter NO_FIELD if it does not
!! exist. If the first character of the named field is the list peparator,
!! then the named field will be relative to the root of the field tree.
!! Otherwise the named field will be relative to the current list.
!> @returns index of the named field if it exists, otherwise the parameter NO_FIELD
function  fm_get_index(name)                        &
          result (index)
integer        :: index
character(len=*), intent(in) :: name !< The name of a field that the user wishes to get an index for

type (field_def), pointer, save :: temp_field_p

if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') then
  index = NO_FIELD
  return
endif
temp_field_p => get_field(name, current_list_p)
if (associated(temp_field_p)) then
  index = temp_field_p%index
else
  index = NO_FIELD
endif
end function  fm_get_index

!> @brief A function to return the full path of the current list.
!!
!> This function returns the full path for the current list. A blank
!! path indicates an error condition has occurred.
!> @returns The path corresponding to the current list
function  fm_get_current_list()                                        &
          result (path)
character(len=fm_path_name_len) :: path

type (field_def), pointer, save :: temp_list_p
if (.not. module_is_initialized) call field_manager_init
temp_list_p => current_list_p
path = ' '

do while (associated(temp_list_p))
  if (temp_list_p%name .eq. ' ') then
    exit
  endif
  path = list_sep // trim(temp_list_p%name) // path
  temp_list_p => temp_list_p%parent
enddo

if (.not. associated(temp_list_p)) then
  path = ' '
elseif (path .eq. ' ') then
  path = list_sep
endif

end function  fm_get_current_list

!> @brief A function to return how many elements are contained within the named
!! list or entry.
!!
!> This function returns the list or entry length for the named list or entry.
!! If the named field or entry does not exist, a value of 0 is returned.
!> @returns The number of elements that the field name has.
function  fm_get_length(name)                        &
          result (length)
integer                      :: length
character(len=*), intent(in) :: name !< The name of a list or entry that the user wishes to get the length of

type (field_def), pointer, save :: temp_field_p

if (.not. module_is_initialized) call field_manaager_init
if (name .eq. ' ') then
  length = 0
  return
endif
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  if (temp_field_p%field_type .eq. list_type) then
    length = temp_field_p%length
  else
    length = temp_field_p%max_index
  endif
else
  length = 0
endif
end function  fm_get_length

!> @brief A function to return the type of the named field
!!
!> This function returns the type of the field for name.
!! This indicates whether the named field is a "list" (has children fields),
!! or has values of type "integer", "real", "logical" or "string".
!! If it does not exist it returns a blank string.
!> @returns A string containing the type of the named field
function  fm_get_type(name)                        &
          result (name_field_type)
character(len=8)             :: name_field_type
character(len=*), intent(in) :: name !< The name of a field that the user wishes to find the type of

type (field_def), pointer, save :: temp_field_p

if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') then
  name_field_type = ' '
  return
endif
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  name_field_type = field_type_name(temp_field_p%field_type)
else
  name_field_type = ' '
endif

end function  fm_get_type

!> @returns A flag to indicate whether the function operated with (false) or without
!! (true) errors.
function  fm_get_value_integer(name, value, index)                 &
          result (success)
logical                                :: success
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to get a value for.
integer,          intent(out)          :: value !< The value associated with the named field.
integer,          intent(in), optional :: index !< An optional index to retrieve a single value from an array.

integer                         :: index_t
type (field_def), pointer, save :: temp_field_p

value = 0
success = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(index)) index_t = index
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  if (temp_field_p%field_type .eq. integer_type) then
    if (index_t .ge. 1 .or. index_t .le. temp_field_p%max_index) then
      value = temp_field_p%i_value(index_t)
      success = .true.
    endif
  endif
endif

end function  fm_get_value_integer

!> @returns A flag to indicate whether the function operated with (false) or without
!! (true) errors.
function  fm_get_value_logical(name, value, index)                 &
          result (success)
logical                                :: success
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to get a value for.
logical,          intent(out)          :: value !< The value associated with the named field
integer,          intent(in), optional :: index !< An optional index to retrieve a single value from an array.

integer                         :: index_t
type (field_def), pointer, save :: temp_field_p

value = .false.
success = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(index)) index_t = index
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  if (temp_field_p%field_type .eq. logical_type) then
    if (index_t .ge. 1 .or. index_t .le. temp_field_p%max_index) then
      value = temp_field_p%l_value(index_t)
      success = .true.
    endif
  endif
endif

end function  fm_get_value_logical

!> @returns A flag to indicate whether the function operated with (false) or without
!! (true) errors.
function  fm_get_value_real(name, value, index)                 &
          result (success)
logical                                :: success
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to get a value for.
real,             intent(out)          :: value !< The value associated with the named field
integer,          intent(in), optional :: index !< An optional index to retrieve a single value from an array.

integer                         :: index_t
type (field_def), pointer, save :: temp_field_p

value = 0.0
success = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(index)) index_t = index
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  if (temp_field_p%field_type .eq. real_type) then
    if (index_t .ge. 1 .or. index_t .le. temp_field_p%max_index) then
      value = temp_field_p%r_value(index_t)
      success = .true.
    endif
  endif
endif

end function  fm_get_value_real

!> @returns A flag to indicate whether the function operated with (false) or without
!! (true) errors.
function  fm_get_value_string(name, value, index)                 &
          result (success)
logical                                :: success
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to get a value for.
character(len=*), intent(out)          :: value !< The value associated with the named field
integer,          intent(in), optional :: index !< An optional index to retrieve a single value from an array.

integer                         :: index_t
type (field_def), pointer, save :: temp_field_p

value = ''
success = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(index)) index_t = index
temp_field_p => get_field(name, current_list_p)

if (associated(temp_field_p)) then
  if (temp_field_p%field_type .eq. string_type) then
    if (index_t .ge. 1 .or. index_t .le. temp_field_p%max_index) then
      value = temp_field_p%s_value(index_t)
      success = .true.
    endif
  endif
endif

end function  fm_get_value_string

!> Iterates through the given list
!> @returns A flag to indicate whether the function operated with (FALSE)
!! or without (TRUE) errors
function  fm_loop_over_list_old(list, name, field_type, index)        &
          result (success)
logical                                      :: success
character(len=*),                intent(in)  :: list !< Name of a list to loop over
character(len=*),                intent(out) :: name !< name of a field from list
character(len=fm_type_name_len), intent(out) :: field_type !< type of a list entry
integer,                         intent(out) :: index !< index of the field within the list

type (field_def), pointer, save :: temp_list_p
if (.not. module_is_initialized) call field_manager_init

if (list .eq. loop_list .and. associated(loop_list_p)) then
  loop_list_p => loop_list_p%next
  success = set_list_stuff()
elseif (list .eq. ' ') then
  loop_list = ' '
  loop_list_p => current_list_p%first_field
  success = set_list_stuff()
else
  loop_list = list
  loop_list_p => find_list(loop_list, current_list_p, .false.)
  if (associated(loop_list_p)) then
    loop_list_p => loop_list_p%first_field
    success = set_list_stuff()
  else
    success = .false.
  endif
endif

return

contains

!> If the the pointer matches to the right list,
!! extract the field information.  Used in fm_loop_over_list
!> @returns A flag to indicate whether the function operated with (FALSE)
!! or without (TRUE) errors
function  set_list_stuff()                                                &
          result (success)
  logical        :: success
  if (associated(loop_list_p)) then
    name = loop_list_p%name
    field_type = field_type_name(loop_list_p%field_type)
    index = loop_list_p%index
    success = .true.
  else
    name = ' '
    field_type = ' '
    index = 0
    success = .false.
    loop_list = ' '
  endif

end function  set_list_stuff

end function  fm_loop_over_list_old

!> given a name of the list, prepares an iterator over the list content.
!! If the name of the given list is blank, then the current list is used
subroutine fm_init_loop(loop_list, iter)
  character(len=*)       , intent(in)  :: loop_list !< name of the list to iterate over
  type(fm_list_iter_type), intent(out) :: iter     !< loop iterator

  if (.not.module_is_initialized) call field_manager_init

  if (loop_list==' ') then ! looping over current list
     iter%ptr => current_list_p%first_field
  else
     iter%ptr => find_list(loop_list,current_list_p,.false.)
     if (associated(iter%ptr)) iter%ptr => iter%ptr%first_field
  endif
end subroutine fm_init_loop

!> given a list iterator, returns information about curren list element
!! and advances the iterator to the next list element. At the end of the
!! list, returns FALSE
function fm_loop_over_list_new(iter, name, field_type, index) &
         result (success)
  logical                       :: success
  type (fm_list_iter_type), intent(inout) :: iter !< list iterator
  character(len=*), intent(out) :: name       !< name of the current list item
  character(len=*), intent(out) :: field_type !< type of the field
  integer         , intent(out) :: index      !< index in the list

  if (.not.module_is_initialized) call field_manager_init
  if (associated(iter%ptr)) then
     name       = iter%ptr%name
     field_type = field_type_name(iter%ptr%field_type)
     index      = iter%ptr%index
     success    = .TRUE.
     iter%ptr => iter%ptr%next
  else
     name       = ' '
     field_type = ' '
     index      = 0
     success    = .FALSE.
  endif
end function fm_loop_over_list_new

!> @brief A function to create a new list
!> Allocate and initialize a new list and return the index of the list. If an
!! error occurs return the parameter NO_FIELD.
!> @return integer index of the newly created list
function  fm_new_list(name, create, keep)                        &
          result (index)
integer                                :: index
character(len=*), intent(in)           :: name !< Name of a list that user wishes to create
logical,          intent(in), optional :: create !< If present and true, create the list if it does not exist
logical,          intent(in), optional :: keep !< If present and true, make this list the current list

logical                          :: create_t
logical                          :: keep_t
character(len=fm_path_name_len)  :: path
character(len=fm_field_name_len) :: base
type (field_def), pointer, save  :: temp_list_p

index = NO_FIELD
create_t = .false.
keep_t = .false.
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(create)) create_t = create
if (present(keep)) keep_t = keep
call find_base(name, path, base)

temp_list_p => find_list(path, current_list_p, create_t)

if (associated(temp_list_p)) then
  temp_list_p => make_list(temp_list_p, base)
  if (associated(temp_list_p)) then
    if (keep_t) current_list_p => temp_list_p
    index = temp_list_p%index
  endif
endif

end function  fm_new_list

!> @brief Assigns a given value to a given field
!> @returns An index for the named field
function  fm_new_value_integer(name, value, create, index, append)     &
          result (field_index)
integer                                :: field_index
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to create
                                               !! a value for.
integer,          intent(in)           :: value !< The value that the user wishes to apply to the
                                                !! named field.
logical,          intent(in), optional :: create !< If present and .true., then a value for this
                                                 !! field will be created.
integer,          intent(in), optional :: index !< The index to an array of values that the user
                                                !! wishes to apply a new value.
logical,          intent(in), optional :: append !< If present and .true., then append the value to
      !! an array of the present values. If present and .true., then index cannot be greater than 0.

logical                          :: create_t
integer                          :: i, ier
integer                          :: index_t
integer, pointer, dimension(:)   :: temp_i_value
character(len=fm_path_name_len)  :: path
character(len=fm_field_name_len) :: base
type (field_def), pointer, save  :: temp_list_p
type (field_def), pointer, save  :: temp_field_p

field_index = NO_FIELD
create_t = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(create)) create_t = create
if (present(index) .and. present(append)) then
  if (append .and. index .gt. 0) return
endif
if (present(index)) index_t = index
if (index_t .lt. 0) return
call find_base(name, path, base)
temp_list_p => find_list(path, current_list_p, create_t)

if (associated(temp_list_p)) then
  temp_field_p => find_field(base, temp_list_p)
  if (.not. associated(temp_field_p)) temp_field_p => create_field(temp_list_p, base)
  if (associated(temp_field_p)) then
    if (temp_field_p%field_type == real_type ) then
       field_index = fm_new_value_real(name, real(value), create, index, append)
       return
    else if (temp_field_p%field_type /= integer_type ) then
      temp_field_p%max_index = 0
    endif
    temp_field_p%field_type = integer_type
    if (present(append)) then
      if (append) then
        index_t = temp_field_p%max_index + 1
      endif
    endif
    if (index_t .gt. temp_field_p%max_index + 1) .or. &
        (index_t .eq. 0 .and. temp_field_p%max_index .gt. 0) return
    if (.not. associated(temp_field_p%i_value) .and. index_t .gt. 0) then
      allocate(temp_field_p%i_value(1))
      temp_field_p%max_index = 1
      temp_field_p%array_dim = 1
    elseif (index_t .gt. temp_field_p%array_dim) then
      temp_field_p%array_dim = temp_field_p%array_dim + array_increment
      allocate (temp_i_value(temp_field_p%array_dim))
      do i = 1, temp_field_p%max_index
        temp_i_value(i) = temp_field_p%i_value(i)
      enddo
      if (associated (temp_field_p%i_value)) deallocate(temp_field_p%i_value)
      temp_field_p%i_value => temp_i_value
      temp_field_p%max_index = index_t
    endif
    if (index_t .gt. 0) then
      temp_field_p%i_value(index_t) = value
      if (index_t .gt. temp_field_p%max_index) temp_field_p%max_index = index_t
    endif
    field_index = temp_field_p%index
  endif
endif

end function  fm_new_value_integer

!> @brief Assigns a given value to a given field
!> @returns An index for the named field
function  fm_new_value_logical(name, value, create, index, append) &
          result (field_index)
integer                                :: field_index
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to create
                                               !! a value for.
logical,          intent(in)           :: value !< The value that the user wishes to apply to the
                                                !! named field.
logical,          intent(in), optional :: create !< If present and .true., then a value for this
                                                 !! field will be created.
integer,          intent(in), optional :: index !< The index to an array of values that the user
                                                !! wishes to apply a new value.
logical,          intent(in), optional :: append !< If present and .true., then append the value to
      !! an array of the present values. If present and .true., then index cannot be greater than 0.

character(len=fm_path_name_len)      :: path
character(len=fm_field_name_len)     :: base
integer                              :: i, ier
integer                              :: index_t
logical                              :: create_t
logical, dimension(:), pointer       :: temp_l_value
type (field_def),      pointer, save :: temp_list_p
type (field_def),      pointer, save :: temp_field_p

field_index = NO_FIELD
create_t = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(create)) create_t = create
if (present(index) .and. present(append)) then
  if (append .and. index .gt. 0) return
endif
if (present(index)) index_t = index
if (index_t .lt. 0) return
call find_base(name, path, base)
temp_list_p => find_list(path, current_list_p, create_t)

if (associated(temp_list_p)) then
  temp_field_p => find_field(base, temp_list_p)
  if (.not. associated(temp_field_p)) temp_field_p => create_field(temp_list_p, base)
  if (associated(temp_field_p)) then
    if (temp_field_p%field_type /= logical_type ) temp_field_p%max_index = 0
    temp_field_p%field_type = logical_type
    if (present(append)) then
      if (append) index_t = temp_field_p%max_index + 1
    endif
    if (index_t .gt. temp_field_p%max_index + 1) .or. &
        (index_t .eq. 0 .and. temp_field_p%max_index .gt. 0) return
    if (.not. associated(temp_field_p%l_value) .and. index_t .gt. 0) then
      allocate(temp_field_p%l_value(1))
      temp_field_p%max_index = 1
      temp_field_p%array_dim = 1
    elseif (index_t .gt. temp_field_p%array_dim) then
      temp_field_p%array_dim = temp_field_p%array_dim + array_increment
      allocate (temp_l_value(temp_field_p%array_dim))
      do i = 1, temp_field_p%max_index
        temp_l_value(i) = temp_field_p%l_value(i)
      enddo
      if (associated(temp_field_p%l_value)) deallocate(temp_field_p%l_value)
      temp_field_p%l_value => temp_l_value
      temp_field_p%max_index = index_t
    endif
    if (index_t .gt. 0) then
      temp_field_p%l_value(index_t) = value
      if (index_t .gt. temp_field_p%max_index) temp_field_p%max_index = index_t
    endif
    field_index = temp_field_p%index
  endif
endif

end function  fm_new_value_logical

!> @brief Assigns a given value to a given field
!> @returns An index for the named field
function  fm_new_value_real(name, value, create, index, append) &
          result (field_index)
integer                                :: field_index
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to create
                                               !! a value for.
real,             intent(in)           :: value !< The value that the user wishes to apply to the
                                                !! named field.
logical,          intent(in), optional :: create !< If present and .true., then a value for this
                                                 !! field will be created.
integer,          intent(in), optional :: index !< The index to an array of values that the user
                                                !! wishes to apply a new value.
logical,          intent(in), optional :: append !< If present and .true., then append the value to
      !! an array of the present values. If present and .true., then index cannot be greater than 0.

logical                          :: create_t
integer                          :: i, ier
integer                          :: index_t
real, pointer, dimension(:)      :: temp_r_value
character(len=fm_path_name_len)  :: path
character(len=fm_field_name_len) :: base
type (field_def), pointer, save  :: temp_list_p
type (field_def), pointer, save  :: temp_field_p

field_index = NO_FIELD
create_t = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(create)) create_t = create
if (present(index) .and. present(append)) then
  if (append .and. index .gt. 0) return
endif
if (present(index)) index_t = index
if (index_t .lt. 0) return
call find_base(name, path, base)
temp_list_p => find_list(path, current_list_p, create_t)

if (associated(temp_list_p)) then
  temp_field_p => find_field(base, temp_list_p)
  if (.not. associated(temp_field_p)) temp_field_p => create_field(temp_list_p, base)
  if (associated(temp_field_p)) then
    if (temp_field_p%field_type == integer_type) then
       allocate(temp_field_p%r_value(size(temp_field_p%i_value)))
       do i = 1, size(temp_field_p%i_value)
          temp_field_p%r_value(i) = temp_field_p%i_value(i)
       enddo
       temp_field_p%field_type = real_type
       deallocate(temp_field_p%i_value)
    else if (temp_field_p%field_type /= real_type ) then
      temp_field_p%max_index = 0
    endif
    temp_field_p%field_type = real_type
    if (present(append)) then
      if (append) index_t = temp_field_p%max_index + 1
    endif
    if (index_t .gt. temp_field_p%max_index + 1) .or. &
        (index_t .eq. 0 .and. temp_field_p%max_index .gt. 0) return
    if (.not. associated(temp_field_p%r_value) .and. index_t .gt. 0) then
      allocate(temp_field_p%r_value(1))
      temp_field_p%max_index = 1
      temp_field_p%array_dim = 1
    elseif (index_t .gt. temp_field_p%array_dim) then
      temp_field_p%array_dim = temp_field_p%array_dim + array_increment
      allocate (temp_r_value(temp_field_p%array_dim))
      do i = 1, temp_field_p%max_index
        temp_r_value(i) = temp_field_p%r_value(i)
      enddo
      if (associated(temp_field_p%r_value)) deallocate(temp_field_p%r_value)
      temp_field_p%r_value => temp_r_value
      temp_field_p%max_index = index_t
    endif
    if (index_t .gt. 0) then
      temp_field_p%r_value(index_t) = value
      if (index_t .gt. temp_field_p%max_index) temp_field_p%max_index = index_t
    endif
    field_index = temp_field_p%index
  endif
endif

end function  fm_new_value_real

!> @brief Assigns a given value to a given field
!> @returns An index for the named field
function  fm_new_value_string(name, value, create, index, append) &
          result (field_index)
integer                                :: field_index
character(len=*), intent(in)           :: name !< The name of a field that the user wishes to create
                                               !! a value for.
character(len=*), intent(in)           :: value !< The value that the user wishes to apply to the
                                                !! named field.
logical,          intent(in), optional :: create !< If present and .true., then a value for this
                                                 !! field will be created.
integer,          intent(in), optional :: index !< The index to an array of values that the user
                                                !! wishes to apply a new value.
logical,          intent(in), optional :: append !< If present and .true., then append the value to

character(len=fm_string_len), dimension(:), pointer :: temp_s_value
character(len=fm_path_name_len)                     :: path
character(len=fm_field_name_len)                    :: base
integer                                             :: i, ier
integer                                             :: index_t
logical                                             :: create_t
type (field_def),                     save, pointer :: temp_list_p
type (field_def),                     save, pointer :: temp_field_p

field_index = NO_FIELD
create_t = .false.
index_t = 1
if (.not. module_is_initialized) call field_manager_init
if (name .eq. ' ') return
if (present(create)) create_t = create
if (present(index) .and. present(append)) then
  if (append .and. index .gt. 0) return
endif
if (present(index)) index_t = index
if (index_t .lt. 0) return
call find_base(name, path, base)
temp_list_p => find_list(path, current_list_p, create_t)

if (associated(temp_list_p)) then
  temp_field_p => find_field(base, temp_list_p)
  if (.not. associated(temp_field_p)) temp_field_p => create_field(temp_list_p, base)
  if (associated(temp_field_p)) then
    if (temp_field_p%field_type /= string_type ) temp_field_p%max_index = 0
    temp_field_p%field_type = string_type
    if (present(append)) then
      if (append) then index_t = temp_field_p%max_index + 1
    endif
    if (index_t .gt. temp_field_p%max_index + 1) .or. &
        (index_t .eq. 0 .and. temp_field_p%max_index .gt. 0) return
    if (.not. associated(temp_field_p%s_value) .and. index_t .gt. 0) then
      allocate(temp_field_p%s_value(1))
      temp_field_p%max_index = 1
      temp_field_p%array_dim = 1
    elseif (index_t .gt. temp_field_p%array_dim) then
      temp_field_p%array_dim = temp_field_p%array_dim + array_increment
      allocate (temp_s_value(temp_field_p%array_dim))
      do i = 1, temp_field_p%max_index
        temp_s_value(i) = temp_field_p%s_value(i)
      enddo
      if (associated(temp_field_p%s_value)) deallocate(temp_field_p%s_value)
      temp_field_p%s_value => temp_s_value
      temp_field_p%max_index = index_t
    endif
    if (index_t .gt. 0) then
      temp_field_p%s_value(index_t) = value
      if (index_t .gt. temp_field_p%max_index) temp_field_p%max_index = index_t
    endif
    field_index = temp_field_p%index
  endif
endif

end function  fm_new_value_string

!> Resets the loop variable. For use in conjunction with fm_loop_over_list.
subroutine  fm_reset_loop

if (.not. module_is_initialized) call field_manager_init
loop_list = ' '
nullify(loop_list_p)

end subroutine  fm_reset_loop

!> Return the root list to the value at initialization.
!!
!> For use in conjunction with fm_change_root.
!!
!! Users should use this routine before leaving their routine if they
!! previously used fm_change_root.
subroutine  fm_return_root

if (.not. module_is_initialized) call field_manager_init
root_p%name = save_root_name
root_p%parent => save_root_parent_p
root_p => root
save_root_name = ' '
nullify(save_root_parent_p)

end subroutine  fm_return_root

!> Return a pointer to the field if it exists relative to this_list_p,
!! null otherwise
!! @returns A pointer to the field name
function get_field(name, this_list_p)                                        &
        result (list_p)
type (field_def), pointer        :: list_p
character(len=*), intent(in)     :: name !< The name of a list that the user wishes to get information for
type (field_def), pointer        :: this_list_p !< A pointer to a list that serves as the base point
                                                !! for searching for name
character(len=fm_path_name_len)  :: path
character(len=fm_field_name_len) :: base
type (field_def), pointer, save  :: temp_p

nullify(list_p)
call find_base(name, path, base)
if (path .ne. ' ') then
  temp_p => find_list(path, this_list_p, .false.)
  if (associated(temp_p)) then
    list_p => find_field(base, temp_p)
  else
    nullify(list_p)
  endif
else
  list_p => find_field(base, this_list_p)
endif

end function get_field

!#######################################################################
!#######################################################################

!> This function allows a user to rename a field without modifying the
!! contents of the field.
!!
!> Function to modify the name of a field.
!! Should be used with caution.
!! @returns A flag to indicate whether the function operated with (FALSE) or
!!     without (TRUE) errors.
function fm_modify_name(oldname, newname)                                        &
        result (success)
logical                          :: success
character(len=*), intent(in)     :: oldname !< The name of a field that the user wishes to change
                                            !! the name of
character(len=*), intent(in)     :: newname !< The name that the user wishes to change the name of
                                            !! the field to.

character(len=fm_path_name_len)  :: path
character(len=fm_field_name_len) :: base
type (field_def), pointer, save  :: list_p
type (field_def), pointer, save  :: temp_p

call find_base(oldname, path, base)
success = .false.
if (path .ne. ' ') then
  temp_p => find_list(path, current_list_p, .false.)
  if (associated(temp_p)) then
    list_p => find_field(base, temp_p)
    if (associated(list_p)) then
      list_p%name = newname
      success = .true.
    endif
  else
    nullify(list_p)
  endif
else
  list_p => find_field(base, current_list_p)
  if (associated(list_p)) then
    list_p%name = newname
    success = .true.
  endif
endif

end function fm_modify_name

!> A subroutine to initialize the values of the pointers. This will remove
!! all fields and reset the field tree to only the root field.
subroutine reset_field_tree

root_p => root
field_type_name(integer_type) = 'integer'
field_type_name(list_type) = 'list'
field_type_name(logical_type) = 'logical'
field_type_name(real_type) = 'real'
field_type_name(string_type) = 'string'
root%name = ' '
root%index = 1
root%parent => root_p
root%field_type = list_type
root%length = 0
nullify(root%first_field)
nullify(root%last_field)
root%max_index = 0
root%array_dim = 0
if (associated(root%i_value)) deallocate(root%i_value)
if (associated(root%l_value)) deallocate(root%l_value)
if (associated(root%r_value)) deallocate(root%r_value)
if (associated(root%s_value)) deallocate(root%s_value)
nullify(root%next)
nullify(root%prev)
current_list_p => root
nullify(loop_list_p)
loop_list = ' '
nullify(save_root_parent_p)
save_root_name = ' '

end subroutine reset_field_tree

!> This function creates a new field and returns a pointer to that field.
!!
!> Allocate and initialize a new list in this_list_p list.
!! @return a pointer to the list on success, or a null pointer
!! on failure.
function  make_list(this_list_p, name)                        &
          result (list_p)
type (field_def), pointer    :: list_p
type (field_def), pointer    :: this_list_p !< Base of a list that the user wishes to add a list to
character(len=*), intent(in) :: name !< name of a list that the user wishes to create

type (field_def), pointer, save :: dummy_p

dummy_p => find_field(name, this_list_p )
if (associated(dummy_p)) then
  list_p => dummy_p
  return
endif
nullify(list_p)
list_p => create_field(this_list_p, name)
if (.not. associated(list_p)) then
  nullify(list_p)
  return
endif
list_p%length = 0
list_p%field_type = list_type
if (associated(list_p%i_value)) deallocate(list_p%i_value)
if (associated(list_p%l_value)) deallocate(list_p%l_value)
if (associated(list_p%r_value)) deallocate(list_p%r_value)
if (associated(list_p%s_value)) deallocate(list_p%s_value)

end function  make_list


!> This is a function that provides the capability to return parameters
!! associated with a field in a pair of strings.
!!
!> Given a name return a list of method names and control strings.
!! This function should return strings similar to those in the field
!! table if a comma delimited format is being used.
!> @return A flag to indicate whether the function operated with (FALSE) or
!! without (TRUE) errors
function fm_query_method(name, method_name, method_control)                &
          result (success)
logical                       :: success
character(len=*), intent(in)  :: name !< name of a list that the user wishes to change to
character(len=*), intent(out) :: method_name !< name of a parameter associated with the named field
character(len=*), intent(out) :: method_control !< value of parameters associated with the named field

character(len=fm_path_name_len) :: path
character(len=fm_path_name_len) :: base
character(len=fm_path_name_len) :: name_loc
logical                         :: recursive_t
type (field_def), pointer, save :: temp_list_p
type (field_def), pointer, save :: temp_value_p
type (field_def), pointer, save :: this_field_p

success     = .false.
recursive_t = .true.
method_name = " "
method_control = " "
if (.not. module_is_initialized) call field_manager_init
name_loc = lowercase(name)
call find_base(name_loc, path, base)
temp_list_p => find_list(name_loc, current_list_p, .false.)
if (associated(temp_list_p)) then
  success = query_method(temp_list_p, recursive_t, base, method_name, method_control)
else
  temp_value_p => find_list(path, current_list_p, .false.)
  if (associated(temp_value_p)) then
  this_field_p => temp_value_p%first_field
  do while (associated(this_field_p))
    if ( this_field_p%name == base ) then
      method_name = this_field_p%s_value(1)
      method_control = ""
      success = .true.
      exit
    else
      success = .false.
    endif
    this_field_p => this_field_p%next
  enddo
  endif
endif

end function  fm_query_method

!> A private function that can recursively recover values for parameters
!! associated with a field.
!> @return A flag to indicate whether the function operated with (FALSE) or
!! without (TRUE) errors
recursive function query_method(list_p, recursive, name, method_name, method_control) &
          result (success)
logical :: success
type (field_def), pointer     :: list_p !< A pointer to the field that is of interest
logical,          intent(in)  :: recursive !< A flag to enable recursive searching if true
character(len=*), intent(in)  :: name !<  name of a list that the user wishes to change to
character(len=*), intent(out) :: method_name !< name of a parameter associated with the named field
character(len=*), intent(out) :: method_control !< value of parameters associated with the named field

integer                         :: i
character(len=64)               :: scratch
type (field_def), pointer :: this_field_p

success = .false.
if (.not. associated(list_p)) .or. (list_p%field_type .ne. list_type) return
success = .true.
this_field_p => list_p%first_field
do while (associated(this_field_p))
  select case(this_field_p%field_type)
  case(list_type)
    if (recursive) then
      if (.not. query_method(this_field_p, .true., this_field_p%name, method_name, method_control)) then
        success = .false.
        exit
      else
        method_name = trim(method_name)//trim(this_field_p%name)
      endif
    endif
  case(integer_type)
      write (scratch,*) this_field_p%i_value
      call concat_strings(method_control, comma//trim(this_field_p%name)//' = '//trim(adjustl(scratch)))
  case(logical_type)
      write (scratch,'(l1)')this_field_p%l_value
      call concat_strings(method_control, comma//trim(this_field_p%name)//' = '//trim(adjustl(scratch)))
  case(real_type)
      write (scratch,*) this_field_p%r_value
      call concat_strings(method_control, comma//trim(this_field_p%name)//' = '//trim(adjustl(scratch)))
  case(string_type)
      call concat_strings(method_control, comma//trim(this_field_p%name)//' = '//trim(this_field_p%s_value(1)))
      do i = 2, this_field_p%max_index
         call concat_strings(method_control, comma//trim(this_field_p%s_value(i)))
      enddo
  case default
      success = .false.
      exit
  end select
  this_field_p => this_field_p%next
enddo

end function query_method

!> private function: appends str2 to the end of str1, with length check
subroutine concat_strings(str1,str2)
   character(*), intent(inout) :: str1
   character(*), intent(in)    :: str2

   character(64) :: n1,n2 ! for error reporting

   if (len_trim(str1)+len_trim(str2)>len(str1)) then
      write(n1,*)len(str1)
      write(n2,*)len_trim(str1)+len_trim(str2)
      call mpp_error(FATAL,'length of output string ('//trim(adjustl(n1))&
           //') is not enough for the result of concatenation (len='&
           //trim(adjustl(n2))//')')
   endif
   str1 = trim(str1)//trim(str2)
end subroutine concat_strings

!> A function that allows the user to copy a field and add a suffix to
!! the name of the new field.
!!
!> Given the name of a pre-existing field and a suffix, this function
!! will create a new field. The name of the new field will be that of
!! the old field with a suffix supplied by the user.
!! @return index of the field that has been created by the copy
function fm_copy_list(list_name, suffix, create ) &
         result(index)
integer        :: index
character(len=*), intent(in)           :: list_name !< name of a field that the user wishes to copy
character(len=*), intent(in)           :: suffix !< suffix that will be added to list_name when
                                                 !! field is copied
logical,          intent(in), optional :: create !< flag to create new list if applicable

character(len=fm_string_len), dimension(MAX_FIELD_METHODS) :: control
character(len=fm_string_len), dimension(MAX_FIELD_METHODS) :: method
character(len=fm_string_len)                               :: head
character(len=fm_string_len)                               :: list_name_new
character(len=fm_string_len)                               :: tail
character(len=fm_string_len)                               :: val_str
integer                                                    :: n
integer                                                    :: num_meth
integer                                                    :: val_int
logical                                                    :: found_methods
logical                                                    :: got_value
logical                                                    :: success
logical                                                    :: val_logical
real                                                       :: val_real
type (field_def), pointer, save                            :: temp_field_p
type (field_def), pointer, save                            :: temp_list_p

num_meth= 1
list_name_new = trim(list_name)//trim(suffix)
if (.not. module_is_initialized) call field_manager_init
if (list_name .eq. ' ') then
  temp_list_p => current_list_p
  success = .true.
else
  temp_list_p => find_list(list_name, current_list_p, .false.)
  if (associated(temp_list_p)) then
    success = .true.
  else
    success = .false.
  endif
endif
if (success) then
  method(:) = ' '
  control(:) = ' '
  found_methods = fm_find_methods(trim(list_name), method, control)
  do n = 1, MAX_FIELD_METHODS
    if (LEN_TRIM(method(n)) > 0 ) then
      index = fm_new_list(trim(list_name_new)//list_sep//method(n), create = create)
      call find_base(method(n), head, tail)
      temp_field_p => find_list(trim(list_name)//list_sep//head,temp_list_p, .false.)
      temp_field_p => find_field(tail,temp_field_p)
      select case (temp_field_p%field_type)
        case (integer_type)
          got_value = fm_get_value( trim(list_name)//list_sep//method(n), val_int)
          if ( fm_new_value( trim(list_name_new)//list_sep//method(n), val_int, &
                             create = create, append = .true.) < 0 ) &
            call mpp_error(FATAL, trim(error_header)//'Could not set the '//trim(method(n))//&
                                  ' for '//trim(list_name)//trim(suffix))
        case (logical_type)
          got_value = fm_get_value( trim(list_name)//list_sep//method(n), val_logical)
          if ( fm_new_value( trim(list_name_new)//list_sep//method(n), val_logical, &
                             create = create, append = .true.) < 0 ) &
            call mpp_error(FATAL, trim(error_header)//'Could not set the '//trim(method(n))//&
                                  ' for '//trim(list_name)//trim(suffix))
        case (real_type)
          got_value = fm_get_value( trim(list_name)//list_sep//method(n), val_real)
          if ( fm_new_value( trim(list_name_new)//list_sep//method(n), val_real, &
                             create = create, append = .true.) < 0 ) &
            call mpp_error(FATAL, trim(error_header)//'Could not set the '//trim(method(n))//&
                                  ' for '//trim(list_name)//trim(suffix))
        case (string_type)
          got_value = fm_get_value( trim(list_name)//list_sep//method(n), val_str)
          if ( fm_new_value( trim(list_name_new)//list_sep//method(n), val_str, &
                             create = create, append = .true.) < 0 ) &
            call mpp_error(FATAL, trim(error_header)//'Could not set the '//trim(method(n))//&
                                  ' for '//trim(list_name)//trim(suffix))
        case default
      end select
    endif
  enddo
endif

end function fm_copy_list

!> This function retrieves all the methods associated with a field.
!!
!> This is different from fm_query_method in that this function gets all
!! the methods associated as opposed to 1 method.
!! @return A flag to indicate whether the function operated with (FALSE) or
!!     without (TRUE) errors.
function fm_find_methods(list_name, methods, control ) &
         result(success)
logical                                     :: success
character(len=*), intent(in)                :: list_name !< The name of a list that the user wishes to find methods for
character(len=*), intent(out), dimension(:) :: methods !< An array of the methods associated with list_name
character(len=*), intent(out), dimension(:) :: control !< An array of the parameters associated with methods

integer                         :: num_meth
type (field_def), pointer, save :: temp_list_p
integer                         :: out_unit

success = .true.
num_meth= 1
if (.not. module_is_initialized) call field_manager_init
if (list_name .eq. ' ') then
  temp_list_p => current_list_p
else
  temp_list_p => find_list(list_name, current_list_p, .false.)
  if (.not. associated(temp_list_p)) success = .false.
endif
if (success) then
  success = find_method(temp_list_p, .true., num_meth, methods, control)
endif

end function fm_find_methods

!> Given a field list pointer this function retrieves methods and
!! associated parameters for the field list.
!! @returns A flag to indicate whether the function operated with (FALSE) or
!!     without (TRUE) errors.
recursive function find_method(list_p, recursive, num_meth, method, control)   &
          result (success)
logical                                     :: success
type (field_def), pointer                   :: list_p !< A pointer to the field of interest
logical,          intent(in)                :: recursive !< If true, search recursively for fields
integer,          intent(inout)             :: num_meth !< The number of methods found
character(len=*), intent(out), dimension(:) :: method !< The methods associated with the field pointed to by list_p
character(len=*), intent(out), dimension(:) :: control !< The control parameters for the methods found

character(len=fm_path_name_len) :: scratch
integer                         :: depthp1
integer                         :: first
integer                         :: i
integer                         :: last
integer                         :: n
type (field_def), pointer, save :: this_field_p

success = .false.
if (.not. associated(list_p)) .or. (list_p%field_type .ne. list_type) return
success = .true.
this_field_p => list_p%first_field
do while (associated(this_field_p))
  select case(this_field_p%field_type)
  case(list_type)
      if ( this_field_p%length > 1) then
         do n = num_meth+1, num_meth + this_field_p%length - 1
            write (method(n),'(a,a,a,$)') trim(method(num_meth)), &
                                          trim(this_field_p%name), list_sep
         enddo
         write (method(num_meth),'(a,a,a,$)') trim(method(num_meth)), &
                                              trim(this_field_p%name), list_sep
      else
         write (method(num_meth),'(a,a,a,$)') trim(method(num_meth)), &
                                              trim(this_field_p%name), list_sep
      endif
      success = find_method(this_field_p, .true., num_meth, method, control)
  case(integer_type)
      write (scratch,*) this_field_p%i_value
      call strip_front_blanks(scratch)
      write (method(num_meth),'(a,a)') trim(method(num_meth)), &
              trim(this_field_p%name)
      write (control(num_meth),'(a)') &
              trim(scratch)
      num_meth = num_meth + 1
  case(logical_type)
      write (method(num_meth),'(a,a)') trim(method(num_meth)), &
              trim(this_field_p%name)
      write (control(num_meth),'(l1)') &
              this_field_p%l_value
      num_meth = num_meth + 1
  case(real_type)
      write (scratch,*) this_field_p%r_value
      call strip_front_blanks(scratch)
      write (method(num_meth),'(a,a)') trim(method(num_meth)), &
              trim(this_field_p%name)
      write (control(num_meth),'(a)') &
              trim(scratch)
      num_meth = num_meth + 1
  case(string_type)
      write (method(num_meth),'(a,a)') trim(method(num_meth)), &
              trim(this_field_p%name)
      write (control(num_meth),'(a)') &
               trim(this_field_p%s_value(1))
      do i = 2, this_field_p%max_index
        write (control(num_meth),'(a,a,$)') comma//trim(this_field_p%s_value(i))
      enddo
      num_meth = num_meth + 1
  case default
      success = .false.
      exit
  end select
  this_field_p => this_field_p%next
enddo

end function find_method

end module field_manager_mod
!> @}
! close documentation grouping
