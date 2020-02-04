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
    h = h * (-2048144789) !0x85ebca6b;
    h = ieor(h, shiftr(h, 13))
    h = h * (-1028477387) !0xc2b2ae35;
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

  subroutine MurmurHash3_x86_32 (key, len, seed, out)
    integer, intent(in)            :: len
    character(len=len), intent(in) :: key
    integer(int32), intent(in)     :: seed
    integer(int32), intent(out)    :: out
    integer(int8)                  :: data(len)
    integer                        :: i
    integer                        :: nblocks
    integer(int32)                 :: h1
    integer(int32), parameter      :: c1 = -862048943 !0xcc9e2d51
    integer(int32), parameter      :: c2 = 461845907 !0x1b873593
    integer(int32)                 :: k1

    h1      = seed
    nblocks = len / 4
    data    = transfer(key, data)

    ! body
    do i = nblocks, 1, -1
       k1 = transfer(data((i-1)*4+1:i*4), k1)

       k1 = k1 * c1;
       k1 = rotl32(k1,15_int8);
       k1 = k1 * c2;

       h1 = ieor(h1, k1);
       h1 = rotl32(h1,13_int8);
       h1 = h1*5-430675100 !0xe6546b64
    end do

    ! tail
    k1 = 0;

    select case (iand(len, 3))
    case (3)
       k1 = ieor(k1, shiftl(int(data(4*nblocks+3), int32), 16))
       k1 = ieor(k1, shiftl(int(data(4*nblocks+2), int32), 8))
       k1 = ieor(k1, int(data(4*nblocks+1), int32))
    case (2)
       k1 = ieor(k1, shiftl(int(data(4*nblocks+2), int32), 8))
       k1 = ieor(k1, int(data(4*nblocks+1), int32))
    case (1)
       k1 = ieor(k1, int(data(4*nblocks+1), int32))
    end select

    k1 = k1 * c1;
    k1 = rotl32(k1,15_int8);
    k1 = k1 * c2;
    h1 = ieor(h1, k1);

    ! finalization
    h1 = ieor(h1, len);
    h1 = fmix32(h1);
    out = h1;
  end subroutine MurmurHash3_x86_32

  ! subroutine MurmurHash3_x86_128 (key, len, seed, out)
  !   const void * key
  !   const integer :: len
  !   integer(int32) :: seed
  !   void * out

  !   const uint8_t * data = (const uint8_t*)key;
  !   const integer :: nblocks = len / 16;
  !   integer :: i;

  !   integer(int32) :: h1 = seed;
  !   integer(int32) :: h2 = seed;
  !   integer(int32) :: h3 = seed;
  !   integer(int32) :: h4 = seed;

  !   integer(int32) :: c1 = 0x239b961b; 
  !   integer(int32) :: c2 = 0xab0e9789;
  !   integer(int32) :: c3 = 0x38b34ae5; 
  !   integer(int32) :: c4 = 0xa1e38b93;

  !   !----------
  !   ! body

  !   const uint32_t * blocks = (const uint32_t *)(data + nblocks*16);

  !   for(i = -nblocks; i; i++)
  !   {
  !   integer(int32) :: k1 = getblock(blocks,i*4+0);
  !   integer(int32) :: k2 = getblock(blocks,i*4+1);
  !   integer(int32) :: k3 = getblock(blocks,i*4+2);
  !   integer(int32) :: k4 = getblock(blocks,i*4+3);

  !   k = k * c1; k1  = rotl32(k1,15); k = k * c2; h1 ^= k1;

  !   h1 = rotl32(h1,19); h1 += h2; h1 = h1*5+0x561ccd1b;

  !   k2 *= c2; k2  = rotl32(k2,16); k2 *= c3; h2 ^= k2;

  !   h2 = rotl32(h2,17); h2 += h3; h2 = h2*5+0x0bcaa747;

  !   k3 *= c3; k3  = rotl32(k3,17); k3 *= c4; h3 ^= k3;

  !   h3 = rotl32(h3,15); h3 += h4; h3 = h3*5+0x96cd1c35;

  !   k4 *= c4; k4  = rotl32(k4,18); k4 *= c1; h4 ^= k4;

  !   h4 = rotl32(h4,13); h4 += h1; h4 = h4*5+0x32ac3b17;
  !   }

  !   !----------
  !   ! tail

  !   const uint8_t * tail = (const uint8_t*)(data + nblocks*16);

  !   integer(int32) :: k1 = 0;
  !   integer(int32) :: k2 = 0;
  !   integer(int32) :: k3 = 0;
  !   integer(int32) :: k4 = 0;

  !   switch(len & 15)
  !   {
  !   case 15: k4 ^= tail[14] << 16;
  !   case 14: k4 ^= tail[13] << 8;
  !   case 13: k4 ^= tail[12] << 0;
  !   k4 *= c4; k4  = rotl32(k4,18); k4 *= c1; h4 ^= k4;

  !   case 12: k3 ^= tail[11] << 24;
  !   case 11: k3 ^= tail[10] << 16;
  !   case 10: k3 ^= tail[ 9] << 8;
  !   case  9: k3 ^= tail[ 8] << 0;
  !   k3 *= c3; k3  = rotl32(k3,17); k3 *= c4; h3 ^= k3;

  !   case  8: k2 ^= tail[ 7] << 24;
  !   case  7: k2 ^= tail[ 6] << 16;
  !   case  6: k2 ^= tail[ 5] << 8;
  !   case  5: k2 ^= tail[ 4] << 0;
  !   k2 *= c2; k2  = rotl32(k2,16); k2 *= c3; h2 ^= k2;

  !   case  4: k1 ^= tail[ 3] << 24;
  !   case  3: k1 ^= tail[ 2] << 16;
  !   case  2: k1 ^= tail[ 1] << 8;
  !   case  1: k1 ^= tail[ 0] << 0;
  !   k = k * c1; k1  = rotl32(k1,15); k = k * c2; h1 ^= k1;
  !   };

  !   !----------
  !   ! finalization

  !   h1 ^= len; h2 ^= len; h3 ^= len; h4 ^= len;

  !   h1 += h2; h1 += h3; h1 += h4;
  !   h2 += h1; h3 += h1; h4 += h1;

  !   h1 = fmix32(h1);
  !   h2 = fmix32(h2);
  !   h3 = fmix32(h3);
  !   h4 = fmix32(h4);

  !   h1 += h2; h1 += h3; h1 += h4;
  !   h2 += h1; h3 += h1; h4 += h1;

  !   ((uint32_t*)out)[0] = h1;
  !   ((uint32_t*)out)[1] = h2;
  !   ((uint32_t*)out)[2] = h3;
  !   ((uint32_t*)out)[3] = h4;
  ! end subroutine MurmurHash3_x86_128

  ! subroutine MurmurHash3_x64_128 (key, len, seed, out)
  !   const void * key
  !   const integer :: len
  !   const integer(int32) :: seed
  !   void * out

  !   const uint8_t * data = (const uint8_t*)key;
  !   const integer :: nblocks = len / 16;
  !   integer :: i;

  !   integer(int64) :: h1 = seed;
  !   integer(int64) :: h2 = seed;

  !   integer(int64) :: c1 = BIG_CONSTANT(0x87c37b91114253d5);
  !   integer(int64) :: c2 = BIG_CONSTANT(0x4cf5ad432745937f);

  !   !----------
  !   ! body

  !   const uint64_t * blocks = (const uint64_t *)(data);

  !   for(i = 0; i < nblocks; i++)
  !   {
  !   integer(int64) :: k1 = getblock(blocks,i*2+0);
  !   integer(int64) :: k2 = getblock(blocks,i*2+1);

  !   k = k * c1; k1  = ROTL64(k1,31); k = k * c2; h1 ^= k1;

  !   h1 = ROTL64(h1,27); h1 += h2; h1 = h1*5+0x52dce729;

  !   k2 *= c2; k2  = ROTL64(k2,33); k2 *= c1; h2 ^= k2;

  !   h2 = ROTL64(h2,31); h2 += h1; h2 = h2*5+0x38495ab5;
  !   }

  !   !----------
  !   ! tail

  !   const uint8_t * tail = (const uint8_t*)(data + nblocks*16);

  !   integer(int64) :: k1 = 0;
  !   integer(int64) :: k2 = 0;

  !   switch(len & 15)
  !   {
  !   case 15: k2 ^= (uint64_t)(tail[14]) << 48;
  !   case 14: k2 ^= (uint64_t)(tail[13]) << 40;
  !   case 13: k2 ^= (uint64_t)(tail[12]) << 32;
  !   case 12: k2 ^= (uint64_t)(tail[11]) << 24;
  !   case 11: k2 ^= (uint64_t)(tail[10]) << 16;
  !   case 10: k2 ^= (uint64_t)(tail[ 9]) << 8;
  !   case  9: k2 ^= (uint64_t)(tail[ 8]) << 0;
  !   k2 *= c2; k2  = ROTL64(k2,33); k2 *= c1; h2 ^= k2;

  !   case  8: k1 ^= (uint64_t)(tail[ 7]) << 56;
  !   case  7: k1 ^= (uint64_t)(tail[ 6]) << 48;
  !   case  6: k1 ^= (uint64_t)(tail[ 5]) << 40;
  !   case  5: k1 ^= (uint64_t)(tail[ 4]) << 32;
  !   case  4: k1 ^= (uint64_t)(tail[ 3]) << 24;
  !   case  3: k1 ^= (uint64_t)(tail[ 2]) << 16;
  !   case  2: k1 ^= (uint64_t)(tail[ 1]) << 8;
  !   case  1: k1 ^= (uint64_t)(tail[ 0]) << 0;
  !   k = k * c1; k1  = ROTL64(k1,31); k = k * c2; h1 ^= k1;
  !   };

  !   !----------
  !   ! finalization

  !   h1 ^= len; h2 ^= len;

  !   h1 += h2;
  !   h2 += h1;

  !   h1 = fmix64(h1);
  !   h2 = fmix64(h2);

  !   h1 += h2;
  !   h2 += h1;

  !   ((uint64_t*)out)[0] = h1;
  !   ((uint64_t*)out)[1] = h2;
  ! end subroutine MurmurHash3_x64_128

end module m_murmur3

