! MurmurHash3 was written by Austin Appleby, and is placed in the public
! domain. The author hereby disclaims copyright to this source code.

! Note - The x86 and x64 versions do _not_ produce the same results, as the
! algorithms are optimized for their respective platforms. You can still
! compile and run any of them on any platform, but your performance with the
! non-native version will be less than optimal.
module m_murmur3
  use iso_fortran_env

  implicit none
  private

  public :: MurmurHash3_x86_32
  public :: MurmurHash3_x64_128

contains

  pure integer(int32) function rotl32 (x, r)
    integer(int32), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl32 = ior(shiftl(x, r), shiftr(x, (32 - r)))
  end function rotl32

  pure integer(int64) function rotl64 (x, r)
    integer(int64), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl64 = ior(shiftl(x, r), shiftr(x, (64 - r)))
  end function rotl64

  ! Finalization mix - force all bits of a hash block to avalanche
  pure integer(int32) function fmix32 (h_in) result(h)
    integer(int32), intent(in) :: h_in
    h = h_in
    h = ieor(h, shiftr(h, 16))
    h = h * (-2048144789) !0x85ebca6b
    h = ieor(h, shiftr(h, 13))
    h = h * (-1028477387) !0xc2b2ae35
    h = ieor(h, shiftr(h, 16))
  end function fmix32

  integer(int64) function fmix64 (k_in) result(k)
    integer(int64), intent(in) :: k_in
    k = k_in
    k = ieor(k, shiftr(k, 33))
    k = k * (-49064778989728563_int64) !0xff51afd7ed558ccd
    k = ieor(k, shiftr(k, 33))
    k = k * (-4265267296055464877_int64) !0xc4ceb9fe1a85ec53
    k = ieor(k, shiftr(k, 33))
  end function fmix64

  subroutine MurmurHash3_x86_32(key, len, seed, hash)
    integer, intent(in)            :: len
    character(len=len), intent(in) :: key
    integer(int32), intent(in)     :: seed
    integer(int32), intent(out)    :: hash
    integer(int8)                  :: data(len)
    integer                        :: i, i0, n, nblocks
    integer(int32)                 :: h1, k1
    integer(int32), parameter      :: c1 = -862048943 ! 0xcc9e2d51
    integer(int32), parameter      :: c2 = 461845907  !0x1b873593
    integer, parameter             :: shifts(3) = [0, 8, 16]

    h1      = seed
    nblocks = shiftr(len, 2)
    data    = transfer(key, data)

    ! body
    do i = 1, nblocks
       k1 = transfer(data(i*4-3:i*4), k1)

       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl32(h1,13_int8)
       h1 = h1 * 5 - 430675100  ! 0xe6546b64
    end do

    ! tail
    k1 = 0

    i = iand(len, 3)
    i0 = 4 * nblocks

    do n = i, 1, -1
       k1 = ieor(k1, shiftl(int(data(i0 + n), int32), shifts(n)))
    end do

    if (i >= 1) then
       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, len)
    h1 = fmix32(h1)
    hash = h1
  end subroutine MurmurHash3_x86_32

  subroutine MurmurHash3_x64_128(key, len, seed, hash)
    integer, intent(in)            :: len
    character(len=len), intent(in) :: key
    integer(int32), intent(in)     :: seed
    integer(int32), intent(out)    :: hash(4)
    integer(int8)                  :: data(len)
    integer                        :: i, i0, n, nblocks
    integer(int64)                 :: h1, h2, k1, k2
    ! 0x87c37b91114253d5
    integer(int64), parameter      :: c1         = -8663945395140668459_int64
    ! 0x4cf5ad432745937f
    integer(int64), parameter      :: c2         = 5545529020109919103_int64
    integer, parameter             :: shifts(15) = [(i*8, i=0,7), (i*8, i=0,6)]

    h1      = seed
    h2      = seed
    nblocks = shiftr(len, 4)
    data    = transfer(key, data)

    ! body
    do i = 1, nblocks
       k1 = transfer(data(i*16-15:i*16-8), k1)
       k2 = transfer(data(i*16-7:i*16), k2)

       k1 = k1 * c1
       k1 = rotl64(k1,31_int8)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl64(h1,27_int8)
       h1 = h1 + h2
       h1 = h1 * 5 + 1390208809_int64 ! 0x52dce729

       k2 = k2 * c2
       k2 = rotl64(k2,33_int8)
       k2 = k2 * c1

       h2 = ieor(h2, k2)
       h2 = rotl64(h2,31_int8)
       h2 = h1 + h2
       h2 = h2 * 5 + 944331445 ! 0x38495ab5
    end do

    ! tail
    k1 = 0
    k2 = 0

    i = iand(len, 15)
    i0 = 16 * nblocks
    do n = i, 9, -1
       k2 = ieor(k2, shiftl(int(data(i0 + n), int64), shifts(n)))
    end do

    if (i >= 9) then
       k2 = k2 * c2
       k2  = rotl64(k2,33_int8)
       k2 = k2 * c1
       h2 = ieor(h2, k2)
    end if

    do n = min(i, 8), 1, -1
       k1 = ieor(k1, shiftl(int(data(i0 + n), int64), shifts(n)))
    end do

    if (i >= 1) then
       k1 = k1 * c1
       k1 = rotl64(k1,31_int8)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, int(len, int64))
    h2 = ieor(h2, int(len, int64))

    h1 = h1 + h2
    h2 = h2 + h1

    h1 = fmix64(h1)
    h2 = fmix64(h2)

    h1 = h1 + h2
    h2 = h2 + h1

    hash = transfer([h1, h2], hash)
  end subroutine MurmurHash3_x64_128

end module m_murmur3
