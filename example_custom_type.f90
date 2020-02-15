program example_custom_type
  use iso_fortran_env

  implicit none

  type my_type
     integer :: number
     character(len=100) :: string
  end type my_type

  type(my_type)     :: x
  integer           :: hash
  character(len=20) :: number_string

  if (command_argument_count() /= 2) &
       stop "Give an integer and a string as argument"

  call get_command_argument(1, number_string)
  read(number_string, *) x%number
  call get_command_argument(2, x%string)

  hash = hash_for_custom_type(x)
  write(*, "(A,Z9)") "x86_32: ", hash

contains

  pure function hash_for_custom_type(x) result(hash)
    use m_murmur3
    type(my_type), intent(in) :: x
    integer, parameter        :: klen = ceiling(storage_size(x) * 0.125d0)
    integer, parameter        :: seed = 42
    character(len=klen)       :: buf
    integer                   :: hash

    call MurmurHash3_x86_32(transfer(x, buf), klen, seed, hash)
  end function hash_for_custom_type

end program example_custom_type
