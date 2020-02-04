program example
  use iso_fortran_env
  use m_murmur3

  implicit none

  character(len=100) :: string
  integer            :: hash_32, hash_128(4)

  if (command_argument_count() /= 1) &
       stop "Give a string as argument"

  call get_command_argument(1, string)
  write(*, "(A)") 'Input: "' // trim(string) // '"'

  call MurmurHash3_x86_32(string, len_trim(string), 42, hash_32)
  write(*, "(A,Z9)") "x86_32: ", hash_32
  call MurmurHash3_x64_128(string, len_trim(string), 42, hash_128)
  write(*, "(A,4Z9)") "x64_128:", hash_128

end program example
