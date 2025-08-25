Fortran port of Murmur3 hash
==============

This is a Fortran port of the [Murmur3](http://code.google.com/p/smhasher/wiki/MurmurHash3) hash function, based on the [C port](https://github.com/PeterScott/murmur3) by Peter Scott.

Usage
-----------

Compile `m_murmur3.f90` and include it with `use m_murmur3`. Two hash functions are included:

    # 32 bit hash, low throughput, low latency (good for short small keys)
    MurmurHash3_x86_32 (key, klen, seed, hash)

    # 128 bit hash, 250% higher throughput, similar latency (good for long keys)
    MurmurHash3_x64_128(key, klen, seed, hash)

The arguments are:
* `key`: string you wish to hash. A generic interface is provided, so that you can give either a string (`character(len=klen) :: key`) or a character array (`character :: key(klen)`)
* `klen`: the length of the string
* `seed`: seed for the hash
* `hash`: resulting hash

`MurmurHash3_x86_128` has been omitted, since it is only useful on older x86 hardware.

Links
-------------

* [ffhash](https://github.com/jannisteunissen/ffhash) Fast Fortran Hash table
* [Hash tables on Fortran Wiki](http://fortranwiki.org/fortran/show/Hash+tables)

License and contributing
--------------------

All this code is in the public domain. Murmur3 was created by Austin Appleby, and the C port and general tidying up (that this code is based on) was done by Peter Scott.
