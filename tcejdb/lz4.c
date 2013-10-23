/*
    zlib-like interface to fast block compression (LZ4 or FastLZ) libraries
    Copyright (C) 2010-2013 Exalead SA. (http://www.exalead.com/)

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.

    Remarks/Bugs:
    LZ4 compression library by Yann Collet (yann.collet.73@gmail.com)
    FastLZ compression library by Ariya Hidayat (ariya@kde.org)
    Library encapsulation by Xavier Roche (fastlz@exalead.com)

    ----------------------------------------------------------------------------
  
    LZ4 - Fast LZ compression algorithm
    Copyright (C) 2011-2012, Yann Collet.
    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

       * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - LZ4 homepage : http://fastcompression.blogspot.com/p/lz4.html
    - LZ4 source repository : http://code.google.com/p/lz4/
*/

#include "myconf.h"
#include "lz4.h"

//FIXME currenly fallback LZ4_compressHC into LZ4_compress
#define LZ4_compressHC LZ4_compress
#define ZFAST_USE_LZ4

/* inlining */
#ifndef ZFASTINLINE
#define ZFASTINLINE inline
#endif

#ifdef _MSC_VER
#  define inline __inline             // Visual is not C99, but supports some kind of inline
#  define forceinline __forceinline   
#  include <intrin.h>                 // For Visual 2005
#  if LZ4_ARCH64	// 64-bit
#    pragma intrinsic(_BitScanForward64) // For Visual 2005
#    pragma intrinsic(_BitScanReverse64) // For Visual 2005
#  else
#    pragma intrinsic(_BitScanForward)   // For Visual 2005
#    pragma intrinsic(_BitScanReverse)   // For Visual 2005
#  endif
#else 
#  ifdef __GNUC__
#    define forceinline inline __attribute__((always_inline))
#  else
#    define forceinline inline
#  endif
#endif

///////////////////////////////////////////////////////////////////////////
//                             LZ4 IMPL                                  //
///////////////////////////////////////////////////////////////////////////

//**************************************
// Tuning parameters
//**************************************
// MEMORY_USAGE :
// Memory usage formula : N->2^N Bytes (examples : 10 -> 1KB; 12 -> 4KB ; 16 -> 64KB; 20 -> 1MB; etc.)
// Increasing memory usage improves compression ratio
// Reduced memory usage can improve speed, due to cache effect
// Default value is 14, for 16KB, which nicely fits into Intel x86 L1 cache
#define MEMORY_USAGE 14

// BIG_ENDIAN_NATIVE_BUT_INCOMPATIBLE :
// This will provide a small boost to performance for big endian cpu, but the resulting compressed stream will be incompatible with little-endian CPU.
// You can set this option to 1 in situations where data will remain within closed environment
// This option is useless on Little_Endian CPU (such as x86)
//#define BIG_ENDIAN_NATIVE_BUT_INCOMPATIBLE 1


//**************************************
// CPU Feature Detection
//**************************************
// 32 or 64 bits ?
#if (defined(__x86_64__) || defined(__x86_64) || defined(__amd64__) || defined(__amd64) || defined(__ppc64__) || defined(_WIN64) || defined(__LP64__) || defined(_LP64) )   // Detects 64 bits mode
#  define LZ4_ARCH64 1
#else
#  define LZ4_ARCH64 0
#endif

// Little Endian or Big Endian ?
// Overwrite the #define below if you know your architecture endianess
#if defined (__GLIBC__)
#  include <endian.h>
#  if (__BYTE_ORDER == __BIG_ENDIAN)
#     define LZ4_BIG_ENDIAN 1
#  endif
#elif (defined(_MYBIGEND) || defined(__BIG_ENDIAN__) || defined(__BIG_ENDIAN) || defined(_BIG_ENDIAN)) && !(defined(__LITTLE_ENDIAN__) || defined(__LITTLE_ENDIAN) || defined(_LITTLE_ENDIAN))
#  define LZ4_BIG_ENDIAN 1
#elif defined(__sparc) || defined(__sparc__) \
   || defined(__ppc__) || defined(_POWER) || defined(__powerpc__) || defined(_ARCH_PPC) || defined(__PPC__) || defined(__PPC) || defined(PPC) || defined(__powerpc__) || defined(__powerpc) || defined(powerpc) \
   || defined(__hpux)  || defined(__hppa) \
   || defined(_MIPSEB) || defined(__s390__)
#  define LZ4_BIG_ENDIAN 1
#else
// Little Endian assumed. PDP Endian and other very rare endian format are unsupported.
#endif

// Unaligned memory access is automatically enabled for "common" CPU, such as x86.
// For others CPU, the compiler will be more cautious, and insert extra code to ensure aligned access is respected
// If you know your target CPU supports unaligned memory access, you want to force this option manually to improve performance
#if defined(__ARM_FEATURE_UNALIGNED)
#  define LZ4_FORCE_UNALIGNED_ACCESS 1
#endif

// Define this parameter if your target system or compiler does not support hardware bit count
#if defined(_MSC_VER) && defined(_WIN32_WCE)            // Visual Studio for Windows CE does not support Hardware bit count
#  define LZ4_FORCE_SW_BITCOUNT
#endif

// AIX
#if defined(_AIX)
#  define LZ4_FORCE_UNALIGNED_ACCESS 1
#endif


//**************************************
// Compiler Options
//**************************************
#if __STDC_VERSION__ >= 199901L   // C99
/* "restrict" is a known keyword */
#else
#  define restrict // Disable restrict
#endif

#define GCC_VERSION (__GNUC__ * 100 + __GNUC_MINOR__)

#ifdef _MSC_VER  // Visual Studio
#  include <intrin.h>   // For Visual 2005
#  if LZ4_ARCH64	// 64-bit
#    pragma intrinsic(_BitScanForward64) // For Visual 2005
#    pragma intrinsic(_BitScanReverse64) // For Visual 2005
#  else
#    pragma intrinsic(_BitScanForward)   // For Visual 2005
#    pragma intrinsic(_BitScanReverse)   // For Visual 2005
#  endif
#endif

#ifdef _MSC_VER
#  define lz4_bswap16(x) _byteswap_ushort(x)
#else
#  define lz4_bswap16(x) ((unsigned short int) ((((x) >> 8) & 0xffu) | (((x) & 0xffu) << 8)))
#endif

#if (GCC_VERSION >= 302) || (__INTEL_COMPILER >= 800) || defined(__clang__)
#  define expect(expr,value)    (__builtin_expect ((expr),(value)) )
#else
#  define expect(expr,value)    (expr)
#endif

#define likely(expr)     expect((expr) != 0, 1)
#define unlikely(expr)   expect((expr) != 0, 0)


#define ALLOCATOR(s) calloc(1,s)
#define FREEMEM free
#define MEM_INIT memset


//**************************************
// Basic Types
//**************************************
#if defined(_MSC_VER)    // Visual Studio does not support 'stdint' natively
#  define BYTE	unsigned __int8
#  define U16		unsigned __int16
#  define U32		unsigned __int32
#  define S32		__int32
#  define U64		unsigned __int64
#else
#  include <stdint.h>
#  define BYTE	uint8_t
#  define U16		uint16_t
#  define U32		uint32_t
#  define S32		int32_t
#  define U64		uint64_t
#endif

#ifndef LZ4_FORCE_UNALIGNED_ACCESS
#  pragma pack(push, 1)
#endif

typedef struct _U16_S { U16 v; } U16_S;
typedef struct _U32_S { U32 v; } U32_S;
typedef struct _U64_S { U64 v; } U64_S;

#ifndef LZ4_FORCE_UNALIGNED_ACCESS
#  pragma pack(pop)
#endif

#define A64(x) (((U64_S *)(x))->v)
#define A32(x) (((U32_S *)(x))->v)
#define A16(x) (((U16_S *)(x))->v)


//**************************************
// Constants
//**************************************
#define MINMATCH 4

#define HASH_LOG (MEMORY_USAGE-2)
#define HASHTABLESIZE (1 << HASH_LOG)
#define HASH_MASK (HASHTABLESIZE - 1)

// NOTCOMPRESSIBLE_DETECTIONLEVEL :
// Decreasing this value will make the algorithm skip faster data segments considered "incompressible"
// This may decrease compression ratio dramatically, but will be faster on incompressible data
// Increasing this value will make the algorithm search more before declaring a segment "incompressible"
// This could improve compression a bit, but will be slower on incompressible data
// The default value (6) is recommended
#define NOTCOMPRESSIBLE_DETECTIONLEVEL 6
#define SKIPSTRENGTH (NOTCOMPRESSIBLE_DETECTIONLEVEL>2?NOTCOMPRESSIBLE_DETECTIONLEVEL:2)
#define STACKLIMIT 13
#define HEAPMODE (HASH_LOG>STACKLIMIT)  // Defines if memory is allocated into the stack (local variable), or into the heap (malloc()).
#define COPYLENGTH 8
#define LASTLITERALS 5
#define MFLIMIT (COPYLENGTH+MINMATCH)
#define MINLENGTH (MFLIMIT+1)

#define MAXD_LOG 16
#define MAX_DISTANCE ((1 << MAXD_LOG) - 1)

#define ML_BITS  4
#define ML_MASK  ((1U<<ML_BITS)-1)
#define RUN_BITS (8-ML_BITS)
#define RUN_MASK ((1U<<RUN_BITS)-1)


//**************************************
// Architecture-specific macros
//**************************************
#if LZ4_ARCH64	// 64-bit
#  define STEPSIZE 8
#  define UARCH U64
#  define AARCH A64
#  define LZ4_COPYSTEP(s,d)       A64(d) = A64(s); d+=8; s+=8;
#  define LZ4_COPYPACKET(s,d)     LZ4_COPYSTEP(s,d)
#  define LZ4_SECURECOPY(s,d,e)   if (d<e) LZ4_WILDCOPY(s,d,e)
#  define HTYPE                   U32
#  define INITBASE(base)          const BYTE* const base = ip
#else		// 32-bit
#  define STEPSIZE 4
#  define UARCH U32
#  define AARCH A32
#  define LZ4_COPYSTEP(s,d)       A32(d) = A32(s); d+=4; s+=4;
#  define LZ4_COPYPACKET(s,d)     LZ4_COPYSTEP(s,d); LZ4_COPYSTEP(s,d);
#  define LZ4_SECURECOPY          LZ4_WILDCOPY
#  define HTYPE                   const BYTE*
#  define INITBASE(base)          const int base = 0
#endif

#if (defined(LZ4_BIG_ENDIAN) && !defined(BIG_ENDIAN_NATIVE_BUT_INCOMPATIBLE))
#  define LZ4_READ_LITTLEENDIAN_16(d,s,p) { U16 v = A16(p); v = lz4_bswap16(v); d = (s) - v; }
#  define LZ4_WRITE_LITTLEENDIAN_16(p,i)  { U16 v = (U16)(i); v = lz4_bswap16(v); A16(p) = v; p+=2; }
#else		// Little Endian
#  define LZ4_READ_LITTLEENDIAN_16(d,s,p) { d = (s) - A16(p); }
#  define LZ4_WRITE_LITTLEENDIAN_16(p,v)  { A16(p) = v; p+=2; }
#endif


//**************************************
// Local structures
//**************************************
struct refTables
{
    HTYPE hashTable[HASHTABLESIZE];
};

//**************************************
// Macros
//**************************************
#define LZ4_HASH_FUNCTION(i)	(((i) * 2654435761U) >> ((MINMATCH*8)-HASH_LOG))
#define LZ4_HASH_VALUE(p)		LZ4_HASH_FUNCTION(A32(p))
#define LZ4_WILDCOPY(s,d,e)		do { LZ4_COPYPACKET(s,d) } while (d<e);
#define LZ4_BLINDCOPY(s,d,l)	{ BYTE* e=(d)+l; LZ4_WILDCOPY(s,d,e); d=e; }

#define HASH_POINTER(p)		   (HashTable[HASH_VALUE(p)] + base)
#define DELTANEXT(p)		   chainTable[(size_t)(p) & MAXD_MASK] 
#define GETNEXT(p)			   ((p) - (size_t)DELTANEXT(p))

//****************************
// Private functions
//****************************


static ZFASTINLINE int LZ4_compressBound(int isize)   { return ((isize) + ((isize)/255) + 16); }
#define           LZ4_COMPRESSBOUND(    isize)            ((isize) + ((isize)/255) + 16)

#if LZ4_ARCH64

static ZFASTINLINE int LZ4_NbCommonBytes (register U64 val)
{
#if defined(LZ4_BIG_ENDIAN)
    #if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
    unsigned long r = 0;
    _BitScanReverse64( &r, val );
    return (int)(r>>3);
    #elif defined(__GNUC__) && (GCC_VERSION >= 304) && !defined(LZ4_FORCE_SW_BITCOUNT)
    return (__builtin_clzll(val) >> 3);
    #else
    int r;
    if (!(val>>32)) { r=4; } else { r=0; val>>=32; }
    if (!(val>>16)) { r+=2; val>>=8; } else { val>>=24; }
    r += (!val);
    return r;
    #endif
#else
    #if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
    unsigned long r = 0;
    _BitScanForward64( &r, val );
    return (int)(r>>3);
    #elif defined(__GNUC__) && (GCC_VERSION >= 304) && !defined(LZ4_FORCE_SW_BITCOUNT)
    return (__builtin_ctzll(val) >> 3);
    #else
    static const int DeBruijnBytePos[64] = { 0, 0, 0, 0, 0, 1, 1, 2, 0, 3, 1, 3, 1, 4, 2, 7, 0, 2, 3, 6, 1, 5, 3, 5, 1, 3, 4, 4, 2, 5, 6, 7, 7, 0, 1, 2, 3, 3, 4, 6, 2, 6, 5, 5, 3, 4, 5, 6, 7, 1, 2, 4, 6, 4, 4, 5, 7, 2, 6, 5, 7, 6, 7, 7 };
    return DeBruijnBytePos[((U64)((val & -val) * 0x0218A392CDABBD3F)) >> 58];
    #endif
#endif
}

#else

static ZFASTINLINE int LZ4_NbCommonBytes (register U32 val)
{
#if defined(LZ4_BIG_ENDIAN)
    #if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
    unsigned long r = 0;
    _BitScanReverse( &r, val );
    return (int)(r>>3);
    #elif defined(__GNUC__) && (GCC_VERSION >= 304) && !defined(LZ4_FORCE_SW_BITCOUNT)
    return (__builtin_clz(val) >> 3);
    #else
    int r;
    if (!(val>>16)) { r=2; val>>=8; } else { r=0; val>>=24; }
    r += (!val);
    return r;
    #endif
#else
    #if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
    unsigned long r;
    _BitScanForward( &r, val );
    return (int)(r>>3);
    #elif defined(__GNUC__) && (GCC_VERSION >= 304) && !defined(LZ4_FORCE_SW_BITCOUNT)
    return (__builtin_ctz(val) >> 3);
    #else
    static const int DeBruijnBytePos[32] = { 0, 0, 3, 0, 3, 1, 3, 0, 3, 2, 2, 1, 3, 2, 0, 1, 3, 3, 1, 2, 2, 2, 2, 0, 3, 1, 2, 0, 1, 0, 1, 1 };
    return DeBruijnBytePos[((U32)((val & -(S32)val) * 0x077CB531U)) >> 27];
    #endif
#endif
}

#endif



//******************************
// Compression functions
//******************************

// LZ4_compressCtx :
// -----------------
// Compress 'isize' bytes from 'source' into an output buffer 'dest' of maximum size 'maxOutputSize'.
// If it cannot achieve it, compression will stop, and result of the function will be zero.
// return : the number of bytes written in buffer 'dest', or 0 if the compression fails

static ZFASTINLINE int LZ4_compressCtx(void** ctx,
                 const char* source,
                 char* dest,
                 int isize,
                 int maxOutputSize)
{
#if HEAPMODE
    struct refTables *srt = (struct refTables *) (*ctx);
    HTYPE* HashTable;
#else
    HTYPE HashTable[HASHTABLESIZE] = {0};
#endif

    const BYTE* ip = (BYTE*) source;
    INITBASE(base);
    const BYTE* anchor = ip;
    const BYTE* const iend = ip + isize;
    const BYTE* const mflimit = iend - MFLIMIT;
#define matchlimit (iend - LASTLITERALS)

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + maxOutputSize;

    int length;
    const int skipStrength = SKIPSTRENGTH;
    U32 forwardH;


    // Init
    if (isize<MINLENGTH) goto _last_literals;
#if HEAPMODE
    if (*ctx == NULL)
    {
        srt = (struct refTables *) malloc ( sizeof(struct refTables) );
        *ctx = (void*) srt;
    }
    HashTable = (HTYPE*)(srt->hashTable);
    memset((void*)HashTable, 0, sizeof(srt->hashTable));
#else
    (void) ctx;
#endif


    // First Byte
    HashTable[LZ4_HASH_VALUE(ip)] = ip - base;
    ip++; forwardH = LZ4_HASH_VALUE(ip);

    // Main Loop
    for ( ; ; )
    {
        int findMatchAttempts = (1U << skipStrength) + 3;
        const BYTE* forwardIp = ip;
        const BYTE* ref;
        BYTE* token;

        // Find a match
        do {
            U32 h = forwardH;
            int step = findMatchAttempts++ >> skipStrength;
            ip = forwardIp;
            forwardIp = ip + step;

            if unlikely(forwardIp > mflimit) { goto _last_literals; }

            forwardH = LZ4_HASH_VALUE(forwardIp);
            ref = base + HashTable[h];
            HashTable[h] = ip - base;

        } while ((ref < ip - MAX_DISTANCE) || (A32(ref) != A32(ip)));

        // Catch up
        while ((ip>anchor) && (ref>(BYTE*)source) && unlikely(ip[-1]==ref[-1])) { ip--; ref--; }

        // Encode Literal length
        length = (int)(ip - anchor);
        token = op++;
        if unlikely(op + length + (2 + 1 + LASTLITERALS) + (length>>8) > oend) return 0; 		// Check output limit
#ifdef _MSC_VER
        if (length>=(int)RUN_MASK) 
        { 
            int len = length-RUN_MASK; 
            *token=(RUN_MASK<<ML_BITS); 
            if (len>254)
            {
                do { *op++ = 255; len -= 255; } while (len>254);
                *op++ = (BYTE)len; 
                memcpy(op, anchor, length);
                op += length;
                goto _next_match;
            }
            else
            *op++ = (BYTE)len; 
        }
        else *token = (length<<ML_BITS);
#else
        if (length>=(int)RUN_MASK) 
        { 
            int len;
            *token=(RUN_MASK<<ML_BITS); 
            len = length-RUN_MASK; 
            for(; len > 254 ; len-=255) *op++ = 255; 
            *op++ = (BYTE)len; 
        }
        else *token = (length<<ML_BITS);
#endif

        // Copy Literals
        LZ4_BLINDCOPY(anchor, op, length);

_next_match:
        // Encode Offset
        LZ4_WRITE_LITTLEENDIAN_16(op,(U16)(ip-ref));

        // Start Counting
        ip+=MINMATCH; ref+=MINMATCH;    // MinMatch already verified
        anchor = ip;
        while likely(ip<matchlimit-(STEPSIZE-1))
        {
            UARCH diff = AARCH(ref) ^ AARCH(ip);
            if (!diff) { ip+=STEPSIZE; ref+=STEPSIZE; continue; }
            ip += LZ4_NbCommonBytes(diff);
            goto _endCount;
        }
        if (LZ4_ARCH64) if ((ip<(matchlimit-3)) && (A32(ref) == A32(ip))) { ip+=4; ref+=4; }
        if ((ip<(matchlimit-1)) && (A16(ref) == A16(ip))) { ip+=2; ref+=2; }
        if ((ip<matchlimit) && (*ref == *ip)) ip++;
_endCount:

        // Encode MatchLength
        length = (int)(ip - anchor);
        if unlikely(op + (1 + LASTLITERALS) + (length>>8) > oend) return 0; 		// Check output limit
        if (length>=(int)ML_MASK) 
        { 
            *token += ML_MASK; 
            length -= ML_MASK; 
            for (; length > 509 ; length-=510) { *op++ = 255; *op++ = 255; } 
            if (length > 254) { length-=255; *op++ = 255; } 
            *op++ = (BYTE)length; 
        }
        else *token += length;

        // Test end of chunk
        if (ip > mflimit) { anchor = ip;  break; }

        // Fill table
        HashTable[LZ4_HASH_VALUE(ip-2)] = ip - 2 - base;

        // Test next position
        ref = base + HashTable[LZ4_HASH_VALUE(ip)];
        HashTable[LZ4_HASH_VALUE(ip)] = ip - base;
        if ((ref > ip - (MAX_DISTANCE + 1)) && (A32(ref) == A32(ip))) { token = op++; *token=0; goto _next_match; }

        // Prepare next loop
        anchor = ip++;
        forwardH = LZ4_HASH_VALUE(ip);
    }

_last_literals:
    // Encode Last Literals
    {
        int lastRun = (int)(iend - anchor);
        if (((char*)op - dest) + lastRun + 1 + ((lastRun+255-RUN_MASK)/255) > (U32)maxOutputSize) return 0;
        if (lastRun>=(int)RUN_MASK) { *op++=(RUN_MASK<<ML_BITS); lastRun-=RUN_MASK; for(; lastRun > 254 ; lastRun-=255) *op++ = 255; *op++ = (BYTE) lastRun; }
        else *op++ = (lastRun<<ML_BITS);
        memcpy(op, anchor, iend - anchor);
        op += iend-anchor;
    }

    // End
    return (int) (((char*)op)-dest);
}

// Note : this function is valid only if isize < LZ4_64KLIMIT
#define LZ4_64KLIMIT ((1<<16) + (MFLIMIT-1))
#define HASHLOG64K (HASH_LOG+1)
#define HASH64KTABLESIZE (1U<<HASHLOG64K)
#define LZ4_HASH64K_FUNCTION(i)	(((i) * 2654435761U) >> ((MINMATCH*8)-HASHLOG64K))
#define LZ4_HASH64K_VALUE(p)	LZ4_HASH64K_FUNCTION(A32(p))
static ZFASTINLINE int LZ4_compress64kCtx(void** ctx,
                 const char* source,
                 char* dest,
                 int isize,
                 int maxOutputSize)
{
#if HEAPMODE
    struct refTables *srt = (struct refTables *) (*ctx);
    U16* HashTable;
#else
    U16 HashTable[HASH64KTABLESIZE] = {0};
#endif

    const BYTE* ip = (BYTE*) source;
    const BYTE* anchor = ip;
    const BYTE* const base = ip;
    const BYTE* const iend = ip + isize;
    const BYTE* const mflimit = iend - MFLIMIT;
#define matchlimit (iend - LASTLITERALS)

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + maxOutputSize;

    int len, length;
    const int skipStrength = SKIPSTRENGTH;
    U32 forwardH;


    // Init
    if (isize<MINLENGTH) goto _last_literals;
#if HEAPMODE
    if (*ctx == NULL)
    {
        srt = (struct refTables *) malloc ( sizeof(struct refTables) );
        *ctx = (void*) srt;
    }
    HashTable = (U16*)(srt->hashTable);
    memset((void*)HashTable, 0, sizeof(srt->hashTable));
#else
    (void) ctx;
#endif


    // First Byte
    ip++; forwardH = LZ4_HASH64K_VALUE(ip);

    // Main Loop
    for ( ; ; )
    {
        int findMatchAttempts = (1U << skipStrength) + 3;
        const BYTE* forwardIp = ip;
        const BYTE* ref;
        BYTE* token;

        // Find a match
        do {
            U32 h = forwardH;
            int step = findMatchAttempts++ >> skipStrength;
            ip = forwardIp;
            forwardIp = ip + step;

            if (forwardIp > mflimit) { goto _last_literals; }

            forwardH = LZ4_HASH64K_VALUE(forwardIp);
            ref = base + HashTable[h];
            HashTable[h] = (U16)(ip - base);

        } while (A32(ref) != A32(ip));

        // Catch up
        while ((ip>anchor) && (ref>(BYTE*)source) && (ip[-1]==ref[-1])) { ip--; ref--; }

        // Encode Literal length
        length = (int)(ip - anchor);
        token = op++;
        if unlikely(op + length + (2 + 1 + LASTLITERALS) + (length>>8) > oend) return 0; 		// Check output limit
#ifdef _MSC_VER
        if (length>=(int)RUN_MASK) 
        { 
            int len = length-RUN_MASK; 
            *token=(RUN_MASK<<ML_BITS); 
            if (len>254)
            {
                do { *op++ = 255; len -= 255; } while (len>254);
                *op++ = (BYTE)len; 
                memcpy(op, anchor, length);
                op += length;
                goto _next_match;
            }
            else
            *op++ = (BYTE)len; 
        }
        else *token = (length<<ML_BITS);
#else
        if (length>=(int)RUN_MASK) { *token=(RUN_MASK<<ML_BITS); len = length-RUN_MASK; for(; len > 254 ; len-=255) *op++ = 255; *op++ = (BYTE)len; }
        else *token = (length<<ML_BITS);
#endif

        // Copy Literals
        LZ4_BLINDCOPY(anchor, op, length);

_next_match:
        // Encode Offset
        LZ4_WRITE_LITTLEENDIAN_16(op,(U16)(ip-ref));

        // Start Counting
        ip+=MINMATCH; ref+=MINMATCH;   // MinMatch verified
        anchor = ip;
        while (ip<matchlimit-(STEPSIZE-1))
        {
            UARCH diff = AARCH(ref) ^ AARCH(ip);
            if (!diff) { ip+=STEPSIZE; ref+=STEPSIZE; continue; }
            ip += LZ4_NbCommonBytes(diff);
            goto _endCount;
        }
        if (LZ4_ARCH64) if ((ip<(matchlimit-3)) && (A32(ref) == A32(ip))) { ip+=4; ref+=4; }
        if ((ip<(matchlimit-1)) && (A16(ref) == A16(ip))) { ip+=2; ref+=2; }
        if ((ip<matchlimit) && (*ref == *ip)) ip++;
_endCount:

        // Encode MatchLength
        len = (int)(ip - anchor);
        if unlikely(op + (1 + LASTLITERALS) + (len>>8) > oend) return 0; 		// Check output limit
        if (len>=(int)ML_MASK) { *token+=ML_MASK; len-=ML_MASK; for(; len > 509 ; len-=510) { *op++ = 255; *op++ = 255; } if (len > 254) { len-=255; *op++ = 255; } *op++ = (BYTE)len; }
        else *token += len;

        // Test end of chunk
        if (ip > mflimit) { anchor = ip;  break; }

        // Fill table
        HashTable[LZ4_HASH64K_VALUE(ip-2)] = (U16)(ip - 2 - base);

        // Test next position
        ref = base + HashTable[LZ4_HASH64K_VALUE(ip)];
        HashTable[LZ4_HASH64K_VALUE(ip)] = (U16)(ip - base);
        if (A32(ref) == A32(ip)) { token = op++; *token=0; goto _next_match; }

        // Prepare next loop
        anchor = ip++;
        forwardH = LZ4_HASH64K_VALUE(ip);
    }

_last_literals:
    // Encode Last Literals
    {
        int lastRun = (int)(iend - anchor);
        if (op + lastRun + 1 + (lastRun-RUN_MASK+255)/255 > oend) return 0;
        if (lastRun>=(int)RUN_MASK) { *op++=(RUN_MASK<<ML_BITS); lastRun-=RUN_MASK; for(; lastRun > 254 ; lastRun-=255) *op++ = 255; *op++ = (BYTE) lastRun; }
        else *op++ = (lastRun<<ML_BITS);
        memcpy(op, anchor, iend - anchor);
        op += iend-anchor;
    }

    // End
    return (int) (((char*)op)-dest);
}


int LZ4_compress_limitedOutput(const char* source, 
                               char* dest, 
                               int isize, 
                               int maxOutputSize)
{
#if HEAPMODE
    void* ctx = malloc(sizeof(struct refTables));
    int result;
    if (isize < LZ4_64KLIMIT)
        result = LZ4_compress64kCtx(&ctx, source, dest, isize, maxOutputSize);
    else result = LZ4_compressCtx(&ctx, source, dest, isize, maxOutputSize);
    free(ctx);
    return result;
#else
    if (isize < (int)LZ4_64KLIMIT) return LZ4_compress64kCtx(NULL, source, dest, isize, maxOutputSize);
    return LZ4_compressCtx(NULL, source, dest, isize, maxOutputSize);
#endif
}


int LZ4_compress(const char* source,
                 char* dest,
                 int isize)
{
    return LZ4_compress_limitedOutput(source, dest, isize, LZ4_compressBound(isize));
}


//****************************
// Decompression functions
//****************************

// Note : The decoding functions LZ4_uncompress() and LZ4_uncompress_unknownOutputSize()
//		are safe against "buffer overflow" attack type.
//		They will never write nor read outside of the provided output buffers.
//      LZ4_uncompress_unknownOutputSize() also insures that it will never read outside of the input buffer.
//		A corrupted input will produce an error result, a negative int, indicating the position of the error within input stream.

int LZ4_uncompress(const char* source,
                 char* dest,
                 int osize)
{
    // Local Variables
    const BYTE* restrict ip = (const BYTE*) source;
    const BYTE* ref;

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + osize;
    BYTE* cpy;

    unsigned token;

    size_t dec32table[] = {0, 3, 2, 3, 0, 0, 0, 0};
#if LZ4_ARCH64
    size_t dec64table[] = {0, 0, 0, -1, 0, 1, 2, 3};
#endif


    // Main Loop
    while (1)
    {
        size_t length;

        // get runlength
        token = *ip++;
        if ((length=(token>>ML_BITS)) == RUN_MASK)  { size_t len; for (;(len=*ip++)==255;length+=255){} length += len; }

        // copy literals
        cpy = op+length;
        if (cpy>oend-COPYLENGTH)
        {
            if (cpy != oend) goto _output_error;         // Error : not enough place for another match (min 4) + 5 literals
            memcpy(op, ip, length);
            ip += length;
            break;                                       // EOF
        }
        LZ4_WILDCOPY(ip, op, cpy); ip -= (op-cpy); op = cpy;

        // get offset
        LZ4_READ_LITTLEENDIAN_16(ref,cpy,ip); ip+=2;
        if unlikely(ref < (BYTE* const)dest) goto _output_error;   // Error : offset outside destination buffer

        // get matchlength
        if ((length=(token&ML_MASK)) == ML_MASK) { for (;*ip==255;length+=255) {ip++;} length += *ip++; }

        // copy repeated sequence
        if unlikely((op-ref)<STEPSIZE)
        {
#if LZ4_ARCH64
            size_t dec64 = dec64table[op-ref];
#else
            const int dec64 = 0;
#endif
            op[0] = ref[0];
            op[1] = ref[1];
            op[2] = ref[2];
            op[3] = ref[3];
            op += 4, ref += 4; ref -= dec32table[op-ref];
            A32(op) = A32(ref); 
            op += STEPSIZE-4; ref -= dec64;
        } else { LZ4_COPYSTEP(ref,op); }
        cpy = op + length - (STEPSIZE-4);

        if unlikely(cpy>oend-(COPYLENGTH)-(STEPSIZE-4))
        {
            if (cpy > oend-LASTLITERALS) goto _output_error;    // Error : last 5 bytes must be literals
            LZ4_SECURECOPY(ref, op, (oend-COPYLENGTH));
            while(op<cpy) *op++=*ref++;
            op=cpy;
            continue;
        }
        
        LZ4_WILDCOPY(ref, op, cpy);
        op=cpy;		// correction
    }

    // end of decoding
    return (int) (((char*)ip)-source);

    // write overflow error detected
_output_error:
    return (int) (-(((char*)ip)-source));
}


int LZ4_uncompress_unknownOutputSize(
                const char* source,
                char* dest,
                int isize,
                int maxOutputSize)
{
    // Local Variables
    const BYTE* restrict ip = (const BYTE*) source;
    const BYTE* const iend = ip + isize;
    const BYTE* ref;

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + maxOutputSize;
    BYTE* cpy;

    size_t dec32table[] = {0, 3, 2, 3, 0, 0, 0, 0};
#if LZ4_ARCH64
    size_t dec64table[] = {0, 0, 0, -1, 0, 1, 2, 3};
#endif


    // Special case
    if unlikely(ip==iend) goto _output_error;    // A correctly formed null-compressed LZ4 must have at least one byte (token=0)

    // Main Loop
    while (1) 
    {
        unsigned token;
        size_t length;

        // get runlength
        token = *ip++;
        if ((length=(token>>ML_BITS)) == RUN_MASK) 
        { 
            int s=255; 
            while (likely(ip<iend) && (s==255)) { s=*ip++; length += s; } 
        }

        // copy literals
        cpy = op+length;
        if ((cpy>oend-MFLIMIT) || (ip+length>iend-(2+1+LASTLITERALS)))
        {
            if (cpy > oend) goto _output_error;          // Error : writes beyond output buffer
            if (ip+length != iend) goto _output_error;   // Error : LZ4 format requires to consume all input at this stage (no match within the last 11 bytes, and at least 8 remaining input bytes for another match+literals)
            memcpy(op, ip, length);
            op += length;
            break;                                       // Necessarily EOF, due to parsing restrictions
        }
        LZ4_WILDCOPY(ip, op, cpy); ip -= (op-cpy); op = cpy;

        // get offset
        LZ4_READ_LITTLEENDIAN_16(ref,cpy,ip); ip+=2;
        if unlikely(ref < (BYTE* const)dest) goto _output_error;   // Error : offset outside of destination buffer

        // get matchlength
        if ((length=(token&ML_MASK)) == ML_MASK)    
        { 
            while likely(ip<iend-(LASTLITERALS+1))    // Error : a minimum input bytes must remain for LASTLITERALS + token
            { 
                int s = *ip++; 
                length +=s; 
                if (s==255) continue; 
                break; 
            } 
        }

        // copy repeated sequence
        if unlikely(op-ref<STEPSIZE)
        {
#if LZ4_ARCH64
            size_t dec64 = dec64table[op-ref];
#else
            const int dec64 = 0;
#endif
            op[0] = ref[0];
            op[1] = ref[1];
            op[2] = ref[2];
            op[3] = ref[3];
            op += 4, ref += 4; ref -= dec32table[op-ref];
            A32(op) = A32(ref); 
            op += STEPSIZE-4; ref -= dec64;
        } else { LZ4_COPYSTEP(ref,op); }
        cpy = op + length - (STEPSIZE-4);

        if unlikely(cpy>oend-(COPYLENGTH+(STEPSIZE-4)))
        {
            if (cpy > oend-LASTLITERALS) goto _output_error;    // Error : last 5 bytes must be literals
            LZ4_SECURECOPY(ref, op, (oend-COPYLENGTH));
            while(op<cpy) *op++=*ref++;
            op=cpy;
            continue;
        }

        LZ4_WILDCOPY(ref, op, cpy);
        op=cpy;		// correction
    }

    // end of decoding
    return (int) (((char*)op)-dest);

    // write overflow error detected
_output_error:
    return (int) (-(((char*)ip)-source));
}

///////////////////////////////////////////////////////////////////////////
//                              EOF LZ4                                  //
///////////////////////////////////////////////////////////////////////////

/* undefined because we use the internal one */
#undef fastlzlibReset

/* note: the 5% ratio (/20) is not sufficient - add 66 bytes too */
/* for LZ4, the expansion ratio is smaller, so we keep the biggest one */
#define EXPANSION_RATIO         10
#define EXPANSION_SECURITY      66
#define HEADER_SIZE             16

#define MIN_BLOCK_SIZE          64
#define DEFAULT_BLOCK_SIZE  262144

/* size of blocks to be compressed */
#define BLOCK_SIZE(S) ( (S)->state->block_size )

/* block size, given the power base (0..15 => 1K..32M) */
#define POWER_BASE 10
#define POWER_TO_BLOCK_SIZE(P) ( 1 << ( P + POWER_BASE ) )

/* estimated upper boundary of compressed size */
#define BUFFER_BLOCK_SIZE(S)                                            \
  ( BLOCK_SIZE(S) + BLOCK_SIZE(S) / EXPANSION_RATIO + HEADER_SIZE*2)

/* block types (base ; the lower four bits are used for block size) */
#define BLOCK_TYPE_RAW         (0x10)
#define BLOCK_TYPE_COMPRESSED  (0xc0)
#define BLOCK_TYPE_BAD_MAGIC   (0xffff)

/* fake level for decompression */
#define ZFAST_LEVEL_DECOMPRESS (-2)

/* macros */

/* the stream is used for compressing */
#define ZFAST_IS_COMPRESSING(S) ( (S)->state->level != ZFAST_LEVEL_DECOMPRESS )

/* the stream is used for decompressing */
#define ZFAST_IS_DECOMPRESSING(S) ( !ZFAST_IS_COMPRESSING(S) )

/* no input bytes available */
#define ZFAST_INPUT_IS_EMPTY(S) ( (S)->avail_in == 0 )

/* no output space available */
#define ZFAST_OUTPUT_IS_FULL(S) ( (S)->avail_out == 0 )

/* the stream has pending output available for the client */
#define ZFAST_HAS_BUFFERED_OUTPUT(S)                    \
  ( s->state->outBuffOffs < s->state->dec_size )

/* compress stream */
#define ZFAST_COMPRESS s->state->compress

/* decompress stream */
#define ZFAST_DECOMPRESS s->state->decompress

/* tools */
#define READ_8(adr)  (*(adr))
#define READ_16(adr) ( READ_8(adr) | (READ_8((adr)+1) << 8) )
#define READ_32(adr) ( READ_16(adr) | (READ_16((adr)+2) << 16) )
#define WRITE_8(buff, n) do {                          \
    *((buff))     = (unsigned char) ((n) & 0xff);      \
  } while(0)
#define WRITE_16(buff, n) do {                          \
    *((buff))     = (unsigned char) ((n) & 0xff);       \
    *((buff) + 1) = (unsigned char) ((n) >> 8);         \
  } while(0)
#define WRITE_32(buff, n) do {                  \
    WRITE_16((buff), (n) & 0xffff);             \
    WRITE_16((buff) + 2, (n) >> 16);            \
  } while(0)

/* magic for opaque "state" structure */
static const char MAGIC[8] = {'F', 'a', 's', 't', 'L', 'Z', 0x01, 0};

/* magic for stream (7 bytes with terminating \0) */
static const char* BLOCK_MAGIC = "FastLZ";

/* opaque structure for "state" zlib structure member */
struct internal_state {
  /* magic ; must be BLOCK_MAGIC */
  char magic[8];

  /* compression level or decompression mode (ZFAST_LEVEL_*) */
  int level;

  /* buffered header and data read so far (if inHdrOffs != 0) */
  Bytef inHdr[HEADER_SIZE];
  uInt inHdrOffs;

  /* block size ; must be 2**(POWER_BASE+n) with n < 16 */
  uInt block_size;
  /* block type (BLOCK_TYPE_*) */
  uInt block_type;
  /* current block stream size (input data block except header) */
  uInt str_size;
  /* current output stream size (output data block) */
  uInt dec_size;

  /* buffered data input */
  Bytef *inBuff;
  /* buffered data output */
  Bytef *outBuff;
  /* buffered data offset in inBuff (iff inBuffOffs < str_size)*/
  uInt inBuffOffs;
  /* buffered data offset in outBuff (iff outBuffOffs < dec_size)*/
  uInt outBuffOffs;
  
  /* block compression backend function */
  int (*compress)(int level, const void* input, int length, void* output);

  /* block decompression backend function */
  int (*decompress)(const void* input, int length, void* output, int maxout); 
};

/* our typed internal state */
typedef struct internal_state zfast_stream_internal;

/* code version */
const char* fastlzlibVersion() {
  return FASTLZ_VERSION_STRING;
}

/* get the block size */
int fastlzlibGetBlockSize(zfast_stream *s) {
  if (s != NULL && s->state != NULL) {
    assert(strcmp(s->state->magic, MAGIC) == 0);
    return BLOCK_SIZE(s);
  }
  return 0;
}

const char* fastlzlibGetLastErrorMessage(zfast_stream *s) {
  if (s != NULL && s->msg != NULL) {
    return s->msg;
  } else {
    return NULL;
  }
}

int fastlzlibGetHeaderSize() {
  return HEADER_SIZE;
}

static voidpf default_zalloc(uInt items, uInt size) {
  return malloc(items*size);
}

static void default_zfree(voidpf address) {
  free(address);
}

static voidpf zalloc(zfast_stream *s, uInt items, uInt size) {
  if (s->zalloc != NULL) {
    return s->zalloc(s->opaque, items, size);
  } else {
    return default_zalloc(items, size);
  }
}

static void zfree(zfast_stream *s, voidpf address) {
  if (s->zfree != NULL) {
    s->zfree(s->opaque, address);
  } else {
    default_zfree(address);
  }
}

/* free private fields */
static void fastlzlibFree(zfast_stream *s) {
  if (s != NULL) {
    if (s->state != NULL) {
      assert(strcmp(s->state->magic, MAGIC) == 0);
      if (s->state->inBuff != NULL) {
        zfree(s, s->state->inBuff);
        s->state->inBuff = NULL;
      }
      if (s->state->outBuff != NULL) {
        zfree(s, s->state->outBuff);
        s->state->outBuff = NULL;
      }
      zfree(s, s->state);
      s->state = NULL;
    }
  }
}

/* reset internal state */
static void fastlzlibReset(zfast_stream *s) {
  assert(strcmp(s->state->magic, MAGIC) == 0);
  s->msg = NULL;
  s->state->inHdrOffs = 0;
  s->state->block_type = 0;
  s->state->str_size = 0;
  s->state->dec_size = 0;
  s->state->inBuffOffs = 0;
  s->state->outBuffOffs = 0;
  s->total_in = 0;
  s->total_out = 0;
}

static ZFASTINLINE int fastlzlibGetBlockSizeLevel(int block_size) {
  int i;
  for(i = 0 ; block_size > 1 && ( ( block_size & 1 ) == 0 )
        ; block_size >>= 1, i++);
  if (block_size == 1 && i >= POWER_BASE && i < POWER_BASE + 0xf) {
    return i - POWER_BASE;
  }
  return -1;
}

#ifdef ZFAST_USE_LZ4

/* compression backend for LZ4 */
static int lz4_backend_compress(int level, const void* input, int length,
                                void* output) {
  if (level == Z_BEST_COMPRESSION) {
    return LZ4_compressHC(input, output, length);
  }
  else {
    return LZ4_compress(input, output, length);
  }
}

/* decompression backend for LZ4 */
static int lz4_backend_decompress(const void* input, int length, void* output,
                                  int maxout) {
  return LZ4_uncompress_unknownOutputSize(input, output, length, maxout);
  /* return LZ4_uncompress(input, output, maxout); */
}

#endif

#ifdef ZFAST_USE_FASTLZ

/* compression backend for FastLZ (level adjustment) */
static int fastlz_backend_compress(int level, const void* input, int length,
                                   void* output) {
  /* Level 1 is the fastest compression and generally useful for short data.
     Level 2 is slightly slower but it gives better compression ratio. */
  const int l = level <= Z_BEST_SPEED ? 1 : 2;
  return fastlz_compress_level(l, input, length, output);
}

#define fastlz_backend_decompress fastlz_decompress

#endif

/* initialize private fields */
static int fastlzlibInit(zfast_stream *s, int block_size) {
  if (s != NULL) {
    int code;
    if (fastlzlibGetBlockSizeLevel(block_size) == -1) {
      s->msg = "block size is invalid";
      return Z_STREAM_ERROR;
    }
    s->state = (zfast_stream_internal*)
      zalloc(s, sizeof(zfast_stream_internal), 1);
    strcpy(s->state->magic, MAGIC);
    s->state->compress = NULL;
    s->state->decompress = NULL;
    if ( ( code = fastlzlibSetCompressor(s, COMPRESSOR_DEFAULT) ) != Z_OK) {
      fastlzlibFree(s);
      return code;
    }
    s->state->block_size = (uInt) block_size;
    s->state->inBuff = zalloc(s, BUFFER_BLOCK_SIZE(s), 1);
    s->state->outBuff = zalloc(s, BUFFER_BLOCK_SIZE(s), 1);
    if (s->state->inBuff != NULL && s->state->outBuff != NULL) {
      fastlzlibReset(s);
      return Z_OK;
    }
    s->msg = "memory exhausted";
    fastlzlibFree(s);
  } else {
    return Z_STREAM_ERROR;
  }
  return Z_MEM_ERROR;
}

int fastlzlibCompressInit2(zfast_stream *s, int level, int block_size) {
  const int success = fastlzlibInit(s, block_size);
  if (success == Z_OK) {
    /* default or unrecognized compression level */
    if (level < Z_NO_COMPRESSION || level > Z_BEST_COMPRESSION) {
      level = Z_BEST_COMPRESSION;
    }
    s->state->level = level;
  }
  return success;
}

int fastlzlibCompressInit(zfast_stream *s, int level) {
  return fastlzlibCompressInit2(s, level, DEFAULT_BLOCK_SIZE);
}

int fastlzlibDecompressInit2(zfast_stream *s, int block_size) {
  const int success = fastlzlibInit(s, block_size);
  if (success == Z_OK) {
    s->state->level = ZFAST_LEVEL_DECOMPRESS;
  }
  return success;
}

int fastlzlibDecompressInit(zfast_stream *s) {
  return fastlzlibDecompressInit2(s, DEFAULT_BLOCK_SIZE);
}

void fastlzlibSetCompress(zfast_stream *s,
                          int (*compress)(int level, const void* input,
                                          int length, void* output)) {
  s->state->compress = compress;
}

void fastlzlibSetDecompress(zfast_stream *s,
                            int (*decompress)(const void* input, int length,
                                              void* output, int maxout)) {
  s->state->decompress = decompress;
}

int fastlzlibSetCompressor(zfast_stream *s,
                           zfast_stream_compressor compressor) {
#ifdef ZFAST_USE_LZ4
  if (compressor == COMPRESSOR_LZ4) {
    fastlzlibSetCompress(s, lz4_backend_compress);
    fastlzlibSetDecompress(s, lz4_backend_decompress);
    return Z_OK;
  }
#endif
#ifdef ZFAST_USE_FASTLZ
  if (compressor == COMPRESSOR_FASTLZ) {
    fastlzlibSetCompress(s, fastlz_backend_compress);
    fastlzlibSetDecompress(s, fastlz_backend_decompress);
    return Z_OK;
  }
#endif
  return Z_VERSION_ERROR;
}

int fastlzlibCompressEnd(zfast_stream *s) {
  if (s == NULL) {
    return Z_STREAM_ERROR;
  }
  fastlzlibFree(s);
  return Z_OK;
}

int fastlzlibDecompressEnd(zfast_stream *s) {
  return fastlzlibCompressEnd(s);
}

int fastlzlibCompressReset(zfast_stream *s) {
  if (s == NULL) {
    return Z_STREAM_ERROR;
  }
  fastlzlibReset(s);
  return Z_OK;
}

int fastlzlibDecompressReset(zfast_stream *s) {
  return fastlzlibCompressReset(s);
}

int fastlzlibCompressMemory(zfast_stream *s) {
  if (s == NULL || s->state == NULL) {
    return -1;
  }
  return (int) ( sizeof(zfast_stream_internal) + BUFFER_BLOCK_SIZE(s) * 2 );
}

int fastlzlibDecompressMemory(zfast_stream *s) {
  return fastlzlibCompressMemory(s);
}

static ZFASTINLINE void inSeek(zfast_stream *s, uInt offs) {
  assert(s->avail_in >= offs);
  s->next_in += offs;
  s->avail_in -= offs;
  s->total_in += offs;
}

static ZFASTINLINE void outSeek(zfast_stream *s, uInt offs) {
  assert(s->avail_out >= offs);
  s->next_out += offs;
  s->avail_out -= offs;
  s->total_out += offs;
}

/* write an header to "dest" */
static ZFASTINLINE int fastlz_write_header(Bytef* dest,
                                           uInt type,
                                           uInt block_size,
                                           uInt compressed,
                                           uInt original) {
  const int bs = fastlzlibGetBlockSizeLevel(block_size);
  const int packed_type = type + bs;
  assert(bs >= 0x0 && bs <= 0xf);
  assert( ( type & 0x0f ) == 0);
  
  memcpy(&dest[0], BLOCK_MAGIC, 7);
  WRITE_8(&dest[7], packed_type);
  WRITE_32(&dest[8], compressed);
  WRITE_32(&dest[12], original);
  return HEADER_SIZE;
}

/* read an header from "source" */
static ZFASTINLINE void fastlz_read_header(const Bytef* source,
                                           uInt *type,
                                           uInt *block_size,
                                           uInt *compressed,
                                           uInt *original) {
  if (memcmp(&source[0], BLOCK_MAGIC, 7) == 0) {
    const int packed_type = READ_8(&source[7]);
    const int block_pow = packed_type & 0x0f;
    *type = packed_type & 0xf0;
    *compressed = READ_32(&source[8]);
    *original = READ_32(&source[12]);
    *block_size = POWER_TO_BLOCK_SIZE(block_pow);
  } else {
    *type = BLOCK_TYPE_BAD_MAGIC;
    *compressed =  0;
    *original =  0;
    *block_size = 0;
  }
}

int fastlzlibGetStreamBlockSize(const void* input, int length) {
  uInt block_size = 0;
  if (length >= HEADER_SIZE) {
    uInt block_type;
    uInt str_size;
    uInt dec_size;
    fastlz_read_header((const Bytef*) input, &block_type, &block_size,
                       &str_size, &dec_size);
  }
  return block_size;
}

int fastlzlibGetStreamInfo(const void* input, int length,
                           uInt *compressed_size,
                           uInt *uncompressed_size) {
  if (input != NULL && compressed_size != NULL && uncompressed_size != NULL) {
    if (length >= HEADER_SIZE) {
      uInt block_size = 0;
      uInt block_type;
      fastlz_read_header((const Bytef*) input, &block_type, &block_size,
                         compressed_size, uncompressed_size);
      if (block_size != 0) {
        return Z_OK;
      } else {
        return Z_DATA_ERROR;
      }
    } else {
      return Z_BUF_ERROR;
    }
  } else {
    return Z_STREAM_ERROR;
  }
}

/* helper for fastlz_compress */
static ZFASTINLINE int fastlz_compress_hdr(const zfast_stream *const s,
                                           const void* input, uInt length,
                                           void* output, uInt output_length,
                                           int block_size, int level,
                                           int flush) {
  uInt done = 0;
  Bytef*const output_start = (Bytef*) output;
  if (length > 0) {
    void*const output_data_start = &output_start[HEADER_SIZE];
    uInt type;
    /* compress and fill header after */
    if (length > MIN_BLOCK_SIZE) {
      done = ZFAST_COMPRESS(level, input, length, output_data_start);
      assert(done + HEADER_SIZE*2 <= output_length);
      if (done < length) {
        type = BLOCK_TYPE_COMPRESSED;
      }
      /* compressed version is greater ; use raw data */
      else {
        memcpy(output_data_start, input, length);
        done = length;
        type = BLOCK_TYPE_RAW;
      }
    }
    /* store small chunk as raw data */
    else {
      assert(length + HEADER_SIZE*2 <= output_length);
      memcpy(output_data_start, input, length);
      done = length;
      type = BLOCK_TYPE_RAW;
    }
    /* write back header */
    done += fastlz_write_header(output_start, type, block_size, done, length);
  }
  /* write an EOF marker (empty block with compressed=uncompressed=0) */
  if (flush == Z_FINISH) {
    Bytef*const output_end = &output_start[done];
    done += fastlz_write_header(output_end, BLOCK_TYPE_COMPRESSED, block_size,
                                0, 0);
  }
  assert(done <= output_length);
  return done;
}

/*
 * Compression and decompression processing routine.
 * The only difference with compression is that the input and output are
 * variables (may change with flush)
 */
static ZFASTINLINE int fastlzlibProcess(zfast_stream *const s, const int flush,
                                        const int may_buffer) {
  const Bytef *in = NULL;
  const uInt prev_avail_in = s->avail_in;
  const uInt prev_avail_out = s->avail_out;

  /* returns Z_OK if something was processed, Z_BUF_ERROR otherwise */
#define PROGRESS_OK() ( ( s->avail_in != prev_avail_in                \
                          || s->avail_out != prev_avail_out )         \
                        ? Z_OK : Z_BUF_ERROR )
  
  /* sanity check for next_in/next_out */
  if (s->next_in == NULL && !ZFAST_INPUT_IS_EMPTY(s)) {
    s->msg = "invalid input";
    return Z_STREAM_ERROR;
  }
  else if (s->next_out == NULL && !ZFAST_OUTPUT_IS_FULL(s)) {
    s->msg = "invalid output";
    return Z_STREAM_ERROR;
  }
  
  /* output buffer data to be processed */
  if (ZFAST_HAS_BUFFERED_OUTPUT(s)) {
    /* maximum size that can be copied */
    uInt size = s->state->dec_size - s->state->outBuffOffs;
    if (size > s->avail_out) {
      size = s->avail_out;
    }
    /* copy and seek */
    if (size > 0) {
      memcpy(s->next_out, &s->state->outBuff[s->state->outBuffOffs], size);
      s->state->outBuffOffs += size;
      outSeek(s, size);
    }
    /* and return chunk */
    return PROGRESS_OK();
  }

  /* read next block (note: output buffer is empty here) */
  else if (s->state->str_size == 0) {
    /* for error reporting only */
    uInt block_size = 0;

    /* decompressing: header is present */
    if (ZFAST_IS_DECOMPRESSING(s)) {

      /* sync to a block */
      if (flush == Z_SYNC_FLUSH && s->state->inHdrOffs == 0) {
        return Z_NEED_DICT;
      }
      
      /* if header read in progress or will be in multiple passes (sheesh) */
      if (s->state->inHdrOffs != 0 || s->avail_in < HEADER_SIZE) {
        /* we are to go buffered for the header - check if this is allowed */
        if (s->state->inHdrOffs == 0 && !may_buffer) {
          s->msg = "need more data on input";
          return Z_BUF_ERROR;
        }
        /* copy up to HEADER_SIZE bytes */
        for( ; s->avail_in > 0 && s->state->inHdrOffs < HEADER_SIZE
               ; s->state->inHdrOffs++, inSeek(s, 1)) {
          s->state->inHdr[s->state->inHdrOffs] = *s->next_in;
        }
      }
      /* process header if completed */

      /* header on client region */
      if (s->state->inHdrOffs == 0 && s->avail_in >= HEADER_SIZE) {
        /* peek header */
        uInt block_type;
        uInt str_size;
        uInt dec_size;
        fastlz_read_header(s->next_in, &block_type, &block_size,
                           &str_size, &dec_size);

        /* not buffered: check if we can do the job at once */
        if (!may_buffer) {
          /* input buffer too small */
          if (s->avail_in < str_size) {
            s->msg = "need more data on input";
            return Z_BUF_ERROR;
          }
          /* output buffer too small */
          else if (s->avail_out < dec_size) {
            s->msg = "need more room on output";
            return Z_BUF_ERROR;
          }
        }

        /* apply/eat the header and continue */
        s->state->block_type = block_type;
        s->state->str_size =  str_size;
        s->state->dec_size =  dec_size;
        inSeek(s, HEADER_SIZE);
      }
      /* header in inHdrOffs buffer */
      else if (s->state->inHdrOffs == HEADER_SIZE) {
        /* peek header */
        uInt block_type;
        uInt str_size;
        uInt dec_size;
        assert(may_buffer);  /* impossible at this point */
        fastlz_read_header(s->state->inHdr, &block_type, &block_size,
                           &str_size, &dec_size);
        s->state->block_type = block_type;
        s->state->str_size = str_size;
        s->state->dec_size = dec_size;
        s->state->inHdrOffs = 0;
      }
      /* otherwise, please come back later (header not fully processed) */
      else {
        assert(may_buffer);  /* impossible at this point */
        return PROGRESS_OK();
      }

      /* compressed and uncompressed == 0 : EOF marker */
      if (s->state->str_size == 0 && s->state->dec_size == 0) {
        return Z_STREAM_END;
      }
    }
    /* compressing: fixed input size (unless flush) */
    else {
      uInt block_type = BLOCK_TYPE_COMPRESSED;
      uInt str_size = BLOCK_SIZE(s);

      /* not enough room on input */
      if (str_size > s->avail_in) {
        if (flush > Z_NO_FLUSH) {
          str_size = s->avail_in;
        } else if (!may_buffer) {
          s->msg = "need more data on input";
          return Z_BUF_ERROR;
        }
      }
      
      /* apply/eat the header and continue */
      s->state->block_type = block_type;
      s->state->str_size = str_size;
      s->state->dec_size = 0;  /* yet unknown */
    }
    
    /* output not buffered yet */
    s->state->outBuffOffs = s->state->dec_size;

    /* sanity check */
    if (s->state->block_type == BLOCK_TYPE_BAD_MAGIC) {
      s->msg = "corrupted compressed stream (bad magic)";
      return Z_DATA_ERROR;
    }
    else if (s->state->block_type != BLOCK_TYPE_RAW
             && s->state->block_type != BLOCK_TYPE_COMPRESSED) {
      s->msg = "corrupted compressed stream (illegal block type)";
      return Z_VERSION_ERROR;
    }
    else if (block_size > BLOCK_SIZE(s)) {
      s->msg = "block size too large";
      return Z_VERSION_ERROR;
    }
    else if (s->state->dec_size > BUFFER_BLOCK_SIZE(s)) {
      s->msg = "corrupted compressed stream (illegal decompressed size)";
      return Z_VERSION_ERROR;
    }
    else if (s->state->str_size > BUFFER_BLOCK_SIZE(s)) {
      s->msg = "corrupted compressed stream (illegal stream size)";
      return Z_VERSION_ERROR;
    }
    
    /* direct data fully available (ie. complete compressed block) ? */
    if (s->avail_in >= s->state->str_size) {
      in = s->next_in;
      inSeek(s, s->state->str_size);
    }
    /* otherwise, buffered */
    else {
      s->state->inBuffOffs = 0;
    }
  }

  /* notes: */
  /* - header always processed at this point */
  /* - no output buffer data to be processed (outBuffOffs == 0) */
 
  /* buffered data: copy as much as possible to inBuff until we have the
     block data size */
  if (in == NULL) {
    /* remaining data to copy in input buffer */
    if (s->state->inBuffOffs < s->state->str_size) {
      uInt size = s->state->str_size - s->state->inBuffOffs;
      if (size > s->avail_in) {
        size = s->avail_in;
      }
      if (size > 0) {
        memcpy(&s->state->inBuff[s->state->inBuffOffs], s->next_in, size);
        s->state->inBuffOffs += size;
        inSeek(s, size);
      }
    }
    /* block stream size (ie. compressed one) reached */
    if (s->state->inBuffOffs == s->state->str_size) {
      in = s->state->inBuff;
      /* we are about to eat buffered data */
      s->state->inBuffOffs = 0;
    }
    /* forced flush: adjust str_size */
    else if (ZFAST_IS_COMPRESSING(s) && flush != Z_NO_FLUSH) {
      in = s->state->inBuff;
      s->state->str_size = s->state->inBuffOffs;
      /* we are about to eat buffered data, reset it (now empty) */
      s->state->inBuffOffs = 0;
    }
  }

  /* we have a complete compressed block (str_size) : where to uncompress ? */
  if (in != NULL) {
    Bytef *out = NULL;
    const uInt in_size = s->state->str_size;

    int flush_now = flush;
    /* we are supposed to finish, but we did not eat all data: ignore for now */
    if (flush_now == Z_FINISH && !ZFAST_INPUT_IS_EMPTY(s)) {
      flush_now = Z_NO_FLUSH;
    }

    /* decompressing */
    if (ZFAST_IS_DECOMPRESSING(s)) {
      int done = 0;
      const uInt out_size = s->state->dec_size;

      /* can decompress directly on client memory */
      if (s->avail_out >= s->state->dec_size) {
        out = s->next_out;
        outSeek(s, s->state->dec_size);
        /* no buffer */
        s->state->outBuffOffs = s->state->dec_size;
      }
      /* otherwise in output buffer */
      else {
        out = s->state->outBuff;
        s->state->outBuffOffs = 0;
      }

      /* input eaten */
      s->state->str_size = 0;

      /* rock'in */
      switch(s->state->block_type) {
      case BLOCK_TYPE_COMPRESSED:
        done = ZFAST_DECOMPRESS(in, in_size, out, out_size);
        break;
      case BLOCK_TYPE_RAW:
        if (out_size >= in_size) {
          memcpy(out, in, in_size);
          done = in_size;
        } else {
          done = 0;
        }
        break;
      default:
        assert(0);
        break;
      }
      if (done != (int) s->state->dec_size) {
        s->msg = "unable to decompress block stream";
        return Z_STREAM_ERROR;
      }
    }
    /* compressing */
    else {
      /* note: if < MIN_BLOCK_SIZE, fastlz_compress_hdr will not compress */
      const uInt estimated_dec_size = in_size + in_size / EXPANSION_RATIO
        + EXPANSION_SECURITY;

      /* can compress directly on client memory */
      if (s->avail_out >= estimated_dec_size) {
        const int done = fastlz_compress_hdr(s, in, in_size,
                                             s->next_out, estimated_dec_size,
                                             BLOCK_SIZE(s),
                                             s->state->level,
                                             flush_now);
        /* seek output */
        outSeek(s, done);
        /* no buffer */
        s->state->outBuffOffs = s->state->dec_size;
      }
      /* otherwise in output buffer */
      else {
        const int done = fastlz_compress_hdr(s, in, in_size,
                                             s->state->outBuff,
                                             BUFFER_BLOCK_SIZE(s),
                                             BLOCK_SIZE(s),
                                             s->state->level,
                                             flush_now);
        /* produced size (in outBuff) */
        s->state->dec_size = (uInt) done;
        /* buffered */
        s->state->outBuffOffs = 0;
      }

      /* input eaten */
      s->state->str_size = 0;
    }
  }

  /* new output buffer data to be processed ; same logic as begining */
  if (ZFAST_HAS_BUFFERED_OUTPUT(s)) {
    /* maximum size that can be copied */
    uInt size = s->state->dec_size - s->state->outBuffOffs;
    if (size > s->avail_out) {
      size = s->avail_out;
    }
    /* copy and seek */
    if (size > 0) {
      memcpy(s->next_out, &s->state->outBuff[s->state->outBuffOffs], size);
      s->state->outBuffOffs += size;
      outSeek(s, size);
    }
  }

  /* so far so good */

  /* success and EOF */
  if (flush == Z_FINISH
      && ZFAST_INPUT_IS_EMPTY(s)
      && !ZFAST_HAS_BUFFERED_OUTPUT(s)) {
    if (!ZFAST_IS_DECOMPRESSING(s)) {
      return Z_STREAM_END;
    }
    /* we are supposed to be done in decompressing but did not see any EOF */
    else {
      s->msg = "unexpected EOF";
      return Z_BUF_ERROR;
    }
  }
  /* success */
  else {
    return PROGRESS_OK();
  }
#undef PROGRESS_OK
}

/* same as fastlzlibProcess(), but retry once if first call did not produce any
   data (ie. only modified the state machine) */
static ZFASTINLINE int fastlzlibProcess2(zfast_stream *const s, const int flush,
                                         const int may_buffer) {
  const uInt prev_avail_in = s->avail_in;
  const uInt prev_avail_out = s->avail_out;
  const int success = fastlzlibProcess(s, flush, may_buffer);
  const uInt avail_in = s->avail_in;
  const uInt avail_out = s->avail_out;
  /* successful, ate input, no data on output ? */
  if (success == 0
      && avail_out == prev_avail_out && avail_in != prev_avail_in
      && flush != Z_NO_FLUSH) {
    return fastlzlibProcess(s, flush, may_buffer);
  } else {
    return success;
  }
}

int fastlzlibDecompress2(zfast_stream *s, int flush, const int may_buffer) {
  if (ZFAST_IS_DECOMPRESSING(s)) {
    return fastlzlibProcess2(s, flush, may_buffer);
  } else {
    s->msg = "decompressing function used with a compressing stream";
    return Z_STREAM_ERROR;
  }
}

int fastlzlibDecompress(zfast_stream *s) {
  return fastlzlibDecompress2(s, Z_NO_FLUSH, 1);
}

int fastlzlibCompress2(zfast_stream *s, int flush, const int may_buffer) {
  if (ZFAST_IS_COMPRESSING(s)) {
    return fastlzlibProcess2(s, flush, may_buffer);
  } else {
    s->msg = "compressing function used with a decompressing stream";
    return Z_STREAM_ERROR;
  }
}

int fastlzlibCompress(zfast_stream *s, int flush) {
  return fastlzlibCompress2(s, flush, 1);
}

int fastlzlibIsCompressedStream(const void* input, int length) {
  if (length >= HEADER_SIZE) {
    const Bytef*const in = (const Bytef*) input;
    return fastlzlibGetStreamBlockSize(in, length) != 0 ? Z_OK : Z_DATA_ERROR;
  } else {
    return Z_BUF_ERROR;
  }
}

int fastlzlibDecompressSync(zfast_stream *s) {
  if (ZFAST_IS_DECOMPRESSING(s)) {
    if (ZFAST_HAS_BUFFERED_OUTPUT(s)) {
      /* not in an error state: uncompressed data available in buffer */
      return Z_OK;
    }
    else {
      /* Note: if s->state->str_size == 0, we are not in an error state: the
         next chunk is to be read; However, we check the chunk anyway. */

      /* at least HEADER_SIZE data */
      if (s->avail_in < HEADER_SIZE) {
        s->msg = "need more data on input";
        return Z_BUF_ERROR;
      }
        
      /* in buffered read of the header.. reset to 0 */
      if (s->state->inHdrOffs != 0) {
        s->state->inHdrOffs = 0;
      }
      
      /* seek */
      for( ; s->avail_in >= HEADER_SIZE
             ; s->state->inHdrOffs++, inSeek(s, 1)) {
        const Bytef *const in = s->next_in;
        if (in[0] == BLOCK_MAGIC[0]
            && in[1] == BLOCK_MAGIC[1]
            && in[2] == BLOCK_MAGIC[2]
            && in[3] == BLOCK_MAGIC[3]
            && in[4] == BLOCK_MAGIC[4]
            && in[5] == BLOCK_MAGIC[5]
            && in[6] == BLOCK_MAGIC[6]
            ) {
          const int block_size = fastlzlibGetStreamBlockSize(in, HEADER_SIZE);
          if (block_size != 0) {
            /* successful seek */
            return Z_OK;
          }
        }
      }
      s->msg = "no flush point found";
      return Z_DATA_ERROR;
    }
  } else {
    s->msg = "decompressing function used with a compressing stream";
    return Z_STREAM_ERROR;
  }
}
