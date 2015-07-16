!=========================================================================================
module myType_type
!=========================================================================================
  use json_module
  implicit none

  private

  ! public derived types:
  public :: myType

  ! public operators:
  public :: operator(==)

  ! public subroutines:
  public :: json_add
  public :: json_get

  ! derived type definitions:
  type :: myType
    integer :: iVal = 0
    real    :: rVal = 0
  end type myType

  ! interfaces:
  interface json_add
    module procedure :: json_add_myType
    module procedure :: json_add_myType_vec
  end interface json_add

  interface json_get
    module procedure :: json_get_myType
    module procedure :: json_get_myType_with_path
    module procedure :: json_get_myType_vec
    module procedure :: json_get_myType_vec_with_path
  end interface json_get

  interface myType
    module procedure :: constructor
  end interface myType

  interface operator(==)
    module procedure :: equality_myType_myType
  end interface operator(==)
contains


!=========================================================================================
! constructor:
!
!   Returns a new type(myType).
!
  pure function constructor( iVal, rVal ) result( val )
    integer, intent(in) :: iVal
    real   , intent(in) :: rVal
    type(myType) :: val

    val%iVal = iVal
    val%rVal = rVal
  end function constructor
!=========================================================================================


!=========================================================================================
! equality_myType_myType:
!
!   Returns a new type(myType).
!
  elemental function equality_myType_myType( LHS, RHS ) result( ans )
    type(myType), intent(in) :: LHS
    type(myType), intent(in) :: RHS
    logical :: ans

    ans = (LHS%iVal == RHS%iVal) .and. (LHS%rVal == RHS%rVal)
  end function equality_myType_myType
!=========================================================================================


!=========================================================================================
! json_add_myType:
!
!   Returns this type(myType) as a type(json_value) object.
!
  subroutine json_add_myType( json, name, val )
    type(json_value), pointer    :: json
    character(*)    , intent(in) :: name
    type(myType)    , intent(in) :: val
    ! local variables:
    type(json_value), pointer :: obj

    call json_create_object(obj, name)
    call json_add(json, obj)

    call json_add(obj, 'iVal', val%iVal)
    call json_add(obj, 'rVal', val%rVal)

    nullify(obj)
  end subroutine json_add_myType
!=========================================================================================


!=========================================================================================
! json_add_myType_vec:
!
!   Returns this type(myType) as a type(json_value) object.
!
  subroutine json_add_myType_vec( json, name, val )
    type(json_value), pointer    :: json
    character(*)    , intent(in) :: name
    type(myType)    , intent(in) :: val(:)
    ! local variables:
    type(json_value), pointer :: array
    integer :: i

    ! create json array
    call json_create_array(array, name)
    do i = 1, size(val)
      call json_add(array, '', val(i))
    end do

    ! add json array
    call json_add(json, array)

    ! cleanup
    nullify(array)
  end subroutine json_add_myType_vec
!=========================================================================================


!=========================================================================================
! json_get_myType:
!
!   Attempts to get a type(myType) variable from the json_value.
!
  subroutine json_get_myType( me, val )
    type(json_value), pointer     :: me
    type(myType)    , intent(out) :: val
    ! local variables:
    logical :: found

    call json_get(me, 'iVal', val%iVal, found)
    if (.not.found) return
    call json_get(me, 'rVal', val%rVal, found)
  end subroutine json_get_myType
!=========================================================================================


!=========================================================================================
! json_get_myType_with_path:
!
!   Attempts to get a type(myType) variable from the json_value tree.
!
  subroutine json_get_myType_with_path( me, path, val, found )
    type(json_value), pointer     :: me
    character(*)    , intent(in)  :: path
    type(myType)    , intent(out) :: val
    logical         , intent(out), optional :: found
    ! local variables:
    type(json_value), pointer :: p
    logical :: found_

    nullify(p)

    ! get the json_value at the given path
    call json_get(me=me, path=path, p=p, found=found_)

    if (found_) then
      call json_get(p, val)
      nullify(p)
      if (present(found)) found = .true.
    else
      if (present(found)) then
        found = .false.
        call json_clear_exceptions()
      end if
    end if
  end subroutine json_get_myType_with_path
!=========================================================================================


!=========================================================================================
! json_get_myType_vec:
!
!   Attempts to get a type(myType) vector from the json_value.
!
  subroutine json_get_myType_vec( me, vec )
    type(json_value)             , pointer     :: me
    type(myType)    , allocatable, intent(out) :: vec(:)
    ! local variables:
    logical :: initialized

    initialized = .false.

    if (allocated(vec)) deallocate(vec)

    !the callback function is called for each element of the array:
    call json_get(me, array_callback=get_myType_from_array)
  contains

    ! callback function for type(myType)
    subroutine get_myType_from_array(element, i, count)
      type(json_value), pointer    :: element
      integer         , intent(in) :: i        !index
      integer         , intent(in) :: count    !size of array

      !size the output array:
      if (.not. initialized) then
        allocate(vec(count))
        initialized = .true.
      end if

      !populate the elements:
      call json_get(element, val=vec(i))
    end subroutine get_myType_from_array
  end subroutine json_get_myType_vec
!=========================================================================================


!=========================================================================================
! json_get_myType_vec_with_path
!
!   Attempts to get a type(myType) vector from the json_value located by the
!   given path.
!
  subroutine json_get_myType_vec_with_path(me, path, vec, found)
    type(json_value)                     , pointer     :: me
    character(kind=CK,len=*)             , intent(in)  :: path
    type(myType)            , allocatable, intent(out) :: vec(:)
    logical                              , intent(out), optional :: found
    ! local variables:
    logical     :: initialized

    initialized = .false.

    call json_get(me, path=path, array_callback=get_myType_from_array, found=found)

    ! need to duplicate callback function, no other way
  contains

    ! callback function for type(myType)
    subroutine get_myType_from_array(element, i, count)
      type(json_value), pointer    :: element
      integer         , intent(in) :: i        !index
      integer         , intent(in) :: count    !size of array

      !size the output array:
      if (.not. initialized) then
        allocate(vec(count))
        initialized = .true.
      end if

      !populate the elements:
      call json_get(element, val=vec(i))
    end subroutine get_myType_from_array
  end subroutine json_get_myType_vec_with_path
!=========================================================================================
end module myType_type
!=========================================================================================
