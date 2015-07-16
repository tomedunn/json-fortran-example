!=========================================================================================
program main
!=========================================================================================
  use json_module
  use myType_type
  implicit none
  ! local variables:
  logical :: found
  integer :: i
  type(json_value), pointer :: json
  character(:), allocatable :: str
  type(myType) :: var_get
  type(myType) :: var_set
  type(myType), allocatable :: vec_get(:)
  type(myType), allocatable :: vec_set(:)

  ! initialize json module
  call json_initialize()

  ! initialize json object
  call json_create_object(json, '')


  ! create scalar type(myType) and add it to json object
  var_set = myType(1, 1.0)
  call json_add(json, 'scalar', var_set)


  ! create vector type(myType) and add it to json object
  allocate(vec_set(3))
  do i = 1, size(vec_set)
    vec_set(i) = myType(i, real(i))
  end do
  call json_add(json, 'vector', vec_set)


  ! get scalar type(myType) variable from json object
  call json_get(json, 'scalar', var_get, found=found)
  if (.not.found) then
    write(6,'("Unable to find type(myType)")')
  else
    if (var_get == var_set) then
      write(6,'("Scalar type(myType) successfully retrieved from json object")')
    else
      write(6,'("Error: Retrieved variable does not match input variable!")')
    end if
  end if
  write(6,'(A)') ''

  ! get vector type(myType) variable from json object
  call json_get(json, 'vector', vec_get, found=found)
  if (.not.found) then
    write(6,'("Unable to find type(myType)")')
  else
    if (all(vec_get == vec_set)) then
      write(6,'("Vector type(myType) successfully retrieved from json object")')
    else
      write(6,'("Error: Retrieved variable does not match input variable!")')
      do i = 1, size(vec_get)
        write(6,'("vec("I0")%iVal = "I2  )') i, vec_get(i)%iVal
        write(6,'("vec("I0")%rVal = "F6.3)') i, vec_get(i)%rVal
      end do
    end if
  end if
  write(6,'(A)') ''

  write(6,'(A)') 'Printing final json object...'
  call json_print_to_string(json, str)
  write(6,'(A)') str

!=========================================================================================
end program main
!=========================================================================================
