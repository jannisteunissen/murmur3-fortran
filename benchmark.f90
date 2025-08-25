program benchmark
  use iso_fortran_env
  use m_murmur3

  implicit none

  character(len=2**15) :: string
  integer              :: n, length, hash_32, hash_128(4)
  real(real64)         :: t0, t1, gbs
  integer, parameter   :: n_max = 100*1000*1000

  if (command_argument_count() /= 1) &
       stop "Give string as argument"
  call get_command_argument(1, string)

  length = len_trim(string)
  write(*, "(A,I0)") 'Input length: ', length

  if (length == len(string)) &
       stop "Input truncated, increase maximum length"

  call cpu_time(t0)
  do n = 1, n_max
     call MurmurHash3_x86_32(string, length, 42, hash_32)
  end do
  call cpu_time(t1)

  ! Determine throughput in GB/s
  gbs = length * 0.5_real64**30 * n_max / (t1-t0)

  write(*, "(A,Z9,E10.2,A,E10.2,A)") "x86_32: ", hash_32, &
       n_max/(t1-t0), " hashes/s", gbs, " GB/s"

  call cpu_time(t0)
  do n = 1, n_max
     call MurmurHash3_x64_128(string, length, 42, hash_128)
  end do
  call cpu_time(t1)

  gbs = length * 0.5_real64**30 * n_max / (t1-t0)
  write(*, "(A,4Z9,E10.2,A,E10.2,A)") "x64_128:", hash_128, &
       n_max/(t1-t0), " hashes/s", gbs, " GB/s"

end program benchmark
