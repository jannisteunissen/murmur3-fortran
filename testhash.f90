program testhash
  use iso_fortran_env
  use m_murmur3

  implicit none

  integer           :: n, n_fail, hashes(4), seeds(4)
  character(len=32) :: strings(4), results(4)
  character(len=32)  :: hash_string

  strings(1) = "Hello, world!"
  strings(2) = "Hello, world!"
  strings(3) = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  strings(4) = ""
  seeds   = [1234, 4321, 1234, 1234]
  results = ["FAF6CDB3", "BF505788", "8905AC28", "0F2CC00B"]

  n_fail = 0

  do n = 1, 4
     call MurmurHash3_x86_32(strings(n), len_trim(strings(n)), &
          seeds(n), hashes(1))
     write(hash_string, "(Z8.8)") hashes(1)
     if (hash_string /= results(n)) n_fail = n_fail + 1
  end do

  seeds   = [123, 321, 123, 123]
  results = ["8743ACAD421C8C73D373C3F5F19732FD", &
       "F86D4004CA47F42BB9546C7979200AEE", &
       "BECF7E04DBCF74637751664EF66E73E0", &
       "4CD9597081679D1ABD92F8784BACE33D"]

  do n = 1, 4
     call MurmurHash3_x64_128(strings(n), len_trim(strings(n)), &
          seeds(n), hashes)
     write(hash_string, "(4Z8.8)") hashes
     if (hash_string /= results(n)) n_fail = n_fail + 1
  end do

  write(*, "(A,I0,A,I0)") "Passed tests: ", 8-n_fail, "/", 8

end program testhash
