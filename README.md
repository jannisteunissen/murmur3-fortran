Fortran port of Murmur3 hash
==============

This is a Fortran port of the [Murmur3](http://code.google.com/p/smhasher/wiki/MurmurHash3) hash function, based on the [C port](https://github.com/PeterScott/murmur3) by Peter Scott.

Usage
-----------

Compile `m_murmur3.f90` and include it with `use m_murmur3`. There are three hash functions:

    MurmurHash3_x86_32 (key, len, seed, hash)
    MurmurHash3_x86_128(key, len, seed, hash)
    MurmurHash3_x64_128(key, len, seed, hash)

The arguments are:
* `key`: a pointer to the data you wish to hash
* `len`: the length in bytes
* `seed`: seed for the hash
* `hash`: resulting hash

The hash functions differ in both their internal mechanisms and in their outputs, as documented at https://github.com/PeterScott/murmur3

**MurmurHash3_x86_32** 32 bit hash, low throughput, low latency (good for short small keys)

**MurmurHash3_x86_128** 128 bit hash, 30% higher throughput, 86% lower latency

**MurmurHash3_x64_128** 128 bit hash, 250% higher throughput, similar latency

License and contributing
--------------------

All this code is in the public domain. Murmur3 was created by Austin Appleby, and the C port and general tidying up (that this code is based on) was done by Peter Scott.
