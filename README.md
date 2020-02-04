Fortran port of Murmur3 hash
==============

This is a Fortran port of the [Murmur3](http://code.google.com/p/smhasher/wiki/MurmurHash3) hash function, based on the [C port](https://github.com/PeterScott/murmur3) by Peter Scott.

Usage
-----------

Compile `m_murmur3.f90` and include it with `use m_murmur3`. Two hash functions are included: 

    # 32 bit hash, low throughput, low latency (good for short small keys)
    MurmurHash3_x86_32 (key, len, seed, hash)

    # 128 bit hash, 250% higher throughput, similar latency
    MurmurHash3_x64_128(key, len, seed, hash)

The arguments are:
* `key`: a pointer to the data you wish to hash
* `len`: the length in bytes
* `seed`: seed for the hash
* `hash`: resulting hash

`MurmurHash3_x86_128` has been omitted, since it is only useful on older x86 hardware. 

License and contributing
--------------------

All this code is in the public domain. Murmur3 was created by Austin Appleby, and the C port and general tidying up (that this code is based on) was done by Peter Scott.
