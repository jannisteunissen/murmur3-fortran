program example
  use m_murmur3

  implicit none

  character(len=100) :: string
  integer            :: hash

  if (command_argument_count() /= 1) &
       error stop "Give a string as argument"

  call get_command_argument(1, string)
  call MurmurHash3_x86_32(string, len_trim(string), 42, hash)
  print *, hash

end program example
