/* SDSLib 2.0 -- A C dynamic strings library
 *
 * Copyright (c) 2006-2015, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2015, Oran Agra
 * Copyright (c) 2015, Redis Labs, Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sds.h"
#include "sdsalloc.h"

// sdsHdrSize 根据type返回对应的sds首部大小
static inline int sdsHdrSize(char type) {
    switch (type & SDS_TYPE_MASK) {
    case SDS_TYPE_5:
        return sizeof(struct sdshdr5);
    case SDS_TYPE_8:
        return sizeof(struct sdshdr8);
    case SDS_TYPE_16:
        return sizeof(struct sdshdr16);
    case SDS_TYPE_32:
        return sizeof(struct sdshdr32);
    case SDS_TYPE_64:
        return sizeof(struct sdshdr64);
    }
    return 0;
}

// sdsReqType 根据字符串长度(string_size)来确定需要的sds类型
static inline char sdsReqType(size_t string_size) {
    if (string_size < (1 << 5))
        return SDS_TYPE_5;
    if (string_size < (1 << 8))
        return SDS_TYPE_8;
    if (string_size < (1 << 16))
        return SDS_TYPE_16;
#if (LONG_MAX == LLONG_MAX)
    if (string_size < (1ll << 32))
        return SDS_TYPE_32;
#endif
    return SDS_TYPE_64;
}

/* Create a new sds string with the content specified by the 'init' pointer
 * and 'initlen'.
 * If NULL is used for 'init' the string is initialized with zero bytes.
 *
 * The string is always null-termined (all the sds strings are, always) so
 * even if you create an sds string with:
 *
 * mystring = sdsnewlen("abc",3);
 *
 * You can print the string with printf() as there is an implicit \0 at the
 * end of the string. However the string is binary safe and can contain
 * \0 characters in the middle, as the length is stored in the sds header. */
// sdsnewlen 创建一个新的sds, 其初始内容由init指定, 初始长度由initlen指定.
// 如果init的值为NULL, 那么创建的新的sds会被初始化为指定长度的零字节.
sds sdsnewlen(const void* init, size_t initlen) {
    void* sh;                         // sds首部的起始地址
    sds   s;                          // sds字符串内容的起始地址
    char  type = sdsReqType(initlen); // 由initlen推出合适的sds类型
    /* Empty strings are usually created in order to append. Use type 8
     * since type 5 is not good at this. */
    if (type == SDS_TYPE_5 && initlen == 0)
        type = SDS_TYPE_8;
    int            hdrlen = sdsHdrSize(type);
    unsigned char* fp; /* flags pointer. */

    // hdrlen: sds首部长度
    // initlen: 申请的字符串长度
    // +1: 这个字节用来保存'\0', 这样就可以直接使用printf()来打印sds字符串了
    sh = s_malloc(hdrlen + initlen + 1);
    // init为NULL时, 说明需要创建一个指定长度的空字符串
    if (!init)
        memset(sh, 0, hdrlen + initlen + 1);
    if (sh == NULL)
        return NULL;
    s  = (char*)sh + hdrlen;
    fp = ((unsigned char*)s) - 1;

    // 填充sds首部
    switch (type) {
    case SDS_TYPE_5: {
        *fp = type | (initlen << SDS_TYPE_BITS);
        break;
    }
    case SDS_TYPE_8: {
        SDS_HDR_VAR(8, s);
        sh->len   = initlen;
        sh->alloc = initlen;
        *fp       = type;
        break;
    }
    case SDS_TYPE_16: {
        SDS_HDR_VAR(16, s);
        sh->len   = initlen;
        sh->alloc = initlen;
        *fp       = type;
        break;
    }
    case SDS_TYPE_32: {
        SDS_HDR_VAR(32, s);
        sh->len   = initlen;
        sh->alloc = initlen;
        *fp       = type;
        break;
    }
    case SDS_TYPE_64: {
        SDS_HDR_VAR(64, s);
        sh->len   = initlen;
        sh->alloc = initlen;
        *fp       = type;
        break;
    }
    }
    if (initlen && init)
        memcpy(s, init, initlen);
    s[initlen] = '\0';
    return s;
}

/* Create an empty (zero length) sds string. Even in this case the string
 * always has an implicit null term. */
// sdsempty 创建一个空的sds.
// 其内存结构为:
// uint8_t        len: 0
// uint8_t        alloc: 0
// unsigned char  flags: 1
// char           buf: '\0'
sds sdsempty(void) { return sdsnewlen("", 0); }

/* Create a new sds string starting from a null terminated C string. */
// sdsnew 从一个C风格字符串创建一个sds字符串, sds的 len == alloc == strlen(init)
sds sdsnew(const char* init) {
    size_t initlen = (init == NULL) ? 0 : strlen(init);
    return sdsnewlen(init, initlen);
}

/* Duplicate an sds string. */
// sdsdup 从一个sds字符串创建一个新的sds字符串, 新的sds字符串为传入的s的副本
sds sdsdup(const sds s) { return sdsnewlen(s, sdslen(s)); }

/* Free an sds string. No operation is performed if 's' is NULL. */
// sdsfree 释放sds字符串s占用的内存空间
void sdsfree(sds s) {
    if (s == NULL)
        return;
    s_free((char*)s - sdsHdrSize(s[-1]));
}

/* Set the sds string length to the length as obtained with strlen(), so
 * considering as content only up to the first null term character.
 *
 * This function is useful when the sds string is hacked manually in some
 * way, like in the following example:
 *
 * s = sdsnew("foobar");
 * s[2] = '\0';
 * sdsupdatelen(s);
 * printf("%d\n", sdslen(s));
 *
 * The output will be "2", but if we comment out the call to sdsupdatelen()
 * the output will be "6" as the string was modified but the logical length
 * remains 6 bytes. */
// sdsupdatelen 仅使用strlen(s)来更新s的len字段, 并不会实际改变底层的存储空间
// 理论上这个函数只会使sds的len字段变小.
void sdsupdatelen(sds s) {
    int reallen = strlen(s);
    sdssetlen(s, reallen);
}

/* Modify an sds string in-place to make it empty (zero length).
 * However all the existing buffer is not discarded but set as free space
 * so that next append operations will not require allocations up to the
 * number of bytes previously available. */
// sdsclear 清空s的字符串内容, 但没有释放原来的底层存储空间
void sdsclear(sds s) {
    sdssetlen(s, 0);
    s[0] = '\0';
}

/* Enlarge the free space at the end of the sds string so that the caller
 * is sure that after calling this function can overwrite up to addlen
 * bytes after the end of the string, plus one more byte for nul term.
 *
 * Note: this does not change the *length* of the sds string as returned
 * by sdslen(), but only the free buffer space we have. */
// sdsMakeRoomFor
// 保证s字符串尾后至少有(addlen+1)个字节的空余内存空间(最后一个字节用来保存'\0')
sds sdsMakeRoomFor(sds s, size_t addlen) {
    void*  sh;                  // sds header
    void*  newsh;               // new sds header
    size_t avail = sdsavail(s); // 可获得的空余内存空间
    size_t len;
    size_t newlen;
    char   type;
    char   oldtype = s[-1] & SDS_TYPE_MASK;
    int    hdrlen;

    /* Return ASAP if there is enough space left. */
    // 如果空余内存空间足够, 则直接返回
    if (avail >= addlen)
        return s;

    len    = sdslen(s);
    sh     = (char*)s - sdsHdrSize(oldtype);
    newlen = (len + addlen);
    // 使新分配的底层空间足够大, 而不仅仅是 >= (len + addlen)
    if (newlen < SDS_MAX_PREALLOC)
        newlen *= 2;
    else
        newlen += SDS_MAX_PREALLOC;

    type = sdsReqType(newlen);

    /* Don't use type 5: the user is appending to the string and type 5 is
     * not able to remember empty space, so sdsMakeRoomFor() must be called
     * at every appending operation. */
    // SDS_TYPE_5无法获得底层的存储空间大小,
    // 所以需要将SDS_TYPE_5更新为SDS_TYPE_8
    if (type == SDS_TYPE_5)
        type = SDS_TYPE_8;

    hdrlen = sdsHdrSize(type);
    if (oldtype == type) {
        newsh = s_realloc(sh, hdrlen + newlen + 1);
        if (newsh == NULL)
            return NULL;
        s = (char*)newsh + hdrlen;
    } else {
        /* Since the header size changes, need to move the string forward,
         * and can't use realloc */
        newsh = s_malloc(hdrlen + newlen + 1);
        if (newsh == NULL)
            return NULL;
        memcpy((char*)newsh + hdrlen, s, len + 1);
        s_free(sh);
        s     = (char*)newsh + hdrlen;
        s[-1] = type;
        sdssetlen(s, len);
    }
    sdssetalloc(s, newlen);
    return s;
}

/* Reallocate the sds string so that it has no free space at the end. The
 * contained string remains not altered, but next concatenation operations
 * will require a reallocation.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// sdsRemoveFreeSpace 重新安排s的底层存储空间,
// 以便s的底层存储恰好能够存储s字符串, 并且 没有多余的空闲存储空间.
// 这个函数调用完后, 原来传入的s将不再有效,
// 原来所有使用s的引用都应该使用此函数返回的指针.
sds sdsRemoveFreeSpace(sds s) {
    void*  sh;                              // old sds header
    void*  newsh;                           // new sds header
    char   type;                            // new sds type
    char   oldtype = s[-1] & SDS_TYPE_MASK; // old sds type
    int    hdrlen;                          // new sds header length
    int    oldhdrlen = sdsHdrSize(oldtype); // old sds header length
    size_t len       = sdslen(s);           // old sds's content length
    sh               = (char*)s - oldhdrlen;

    /* Check what would be the minimum SDS header that is just good enough to
     * fit this string. */
    type   = sdsReqType(len);
    hdrlen = sdsHdrSize(type);

    // 需要注意的是理论上type只会降低而不会升高
    // 但是也可能会出现一些特殊情况,
    // 比如用户直接使用sdssetlen()来更新sds的len字段,
    // 但是这样使用会出现一些不可预估的情况, 比如只更新sds的len字段,
    // 却不更新对应的首部.

    /* If the type is the same, or at least a large enough type is still
     * required, we just realloc(), letting the allocator to do the copy
     * only if really needed. Otherwise if the change is huge, we manually
     * reallocate the string to use the different header type. */
    if (oldtype == type || type > SDS_TYPE_8) {
        newsh = s_realloc(sh, oldhdrlen + len + 1);
        if (newsh == NULL)
            return NULL;
        s = (char*)newsh + oldhdrlen;
    } else {
        // oldtype != type && type <= SDS_TYPE_8
        newsh = s_malloc(hdrlen + len + 1);
        if (newsh == NULL)
            return NULL;
        memcpy((char*)newsh + hdrlen, s, len + 1);
        s_free(sh);
        s     = (char*)newsh + hdrlen;
        s[-1] = type;
        sdssetlen(s, len);
    }
    sdssetalloc(s, len);
    return s;
}

/* Return the total size of the allocation of the specifed sds string,
 * including:
 * 1) The sds header before the pointer.
 * 2) The string.
 * 3) The free buffer at the end if any.
 * 4) The implicit null term.
 */
// sdsAllocSize 返回sds对应的底层存储的空间的总体大小, 其中包括:
// 1) sds的首部大小;
// 2) sds的内容所占空间大小;
// 3) sds空余的内存空间大小(如果还有空余空间的话);
// 4) sds中隐含的一个字节的空字符;
size_t sdsAllocSize(sds s) {
    size_t alloc = sdsalloc(s);
    return sdsHdrSize(s[-1]) + alloc + 1;
}

/* Return the pointer of the actual SDS allocation (normally SDS strings
 * are referenced by the start of the string buffer). */
// sdsAllocPtr 返回sds底层存储空间的起始地址, 也就是sds的首部地址
void* sdsAllocPtr(sds s) { return (void*)(s - sdsHdrSize(s[-1])); }

/* Increment the sds length and decrements the left free space at the
 * end of the string according to 'incr'. Also set the null term
 * in the new end of the string.
 *
 * This function is used in order to fix the string length after the
 * user calls sdsMakeRoomFor(), writes something after the end of
 * the current string, and finally needs to set the new length.
 *
 * Note: it is possible to use a negative increment in order to
 * right-trim the string.
 *
 * Usage example:
 *
 * Using sdsIncrLen() and sdsMakeRoomFor() it is possible to mount the
 * following schema, to cat bytes coming from the kernel to the end of an
 * sds string without copying into an intermediate buffer:
 *
 * oldlen = sdslen(s);
 * s = sdsMakeRoomFor(s, BUFFER_SIZE);
 * nread = read(fd, s+oldlen, BUFFER_SIZE);
 * ... check for nread <= 0 and handle it ...
 * sdsIncrLen(s, nread);
 */
// sdsIncrLen 用于给sds的len字段增加incr, incr可以为负.
// incr是有隐含限制的, 即不能更改sds的类型.
void sdsIncrLen(sds s, int incr) {
    unsigned char flags = s[-1];
    size_t        len;
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5: {
        unsigned char* fp     = ((unsigned char*)s) - 1;
        unsigned char  oldlen = SDS_TYPE_5_LEN(flags);
        // assert条件为假时, 向stderr打印错误信息, 并调用abort()来终止进程
        // assert运行时断言
        assert((incr > 0 && oldlen + incr < 32) ||
               (incr < 0 && oldlen >= (unsigned int)(-incr)));
        *fp = SDS_TYPE_5 | ((oldlen + incr) << SDS_TYPE_BITS);
        len = oldlen + incr;
        break;
    }
    case SDS_TYPE_8: {
        SDS_HDR_VAR(8, s);
        assert((incr >= 0 && sh->alloc - sh->len >= incr) ||
               (incr < 0 && sh->len >= (unsigned int)(-incr)));
        len = (sh->len += incr);
        break;
    }
    case SDS_TYPE_16: {
        SDS_HDR_VAR(16, s);
        assert((incr >= 0 && sh->alloc - sh->len >= incr) ||
               (incr < 0 && sh->len >= (unsigned int)(-incr)));
        len = (sh->len += incr);
        break;
    }
    case SDS_TYPE_32: {
        SDS_HDR_VAR(32, s);
        assert((incr >= 0 && sh->alloc - sh->len >= (unsigned int)incr) ||
               (incr < 0 && sh->len >= (unsigned int)(-incr)));
        len = (sh->len += incr);
        break;
    }
    case SDS_TYPE_64: {
        SDS_HDR_VAR(64, s);
        assert((incr >= 0 && sh->alloc - sh->len >= (uint64_t)incr) ||
               (incr < 0 && sh->len >= (uint64_t)(-incr)));
        len = (sh->len += incr);
        break;
    }
    default:
        len = 0; /* Just to avoid compilation warnings. */
    }
    s[len] = '\0';
}

/* Grow the sds to have the specified length. Bytes that were not part of
 * the original length of the sds will be set to zero.
 *
 * if the specified length is smaller than the current length, no operation
 * is performed. */
// sdsgrowzero 将sds的长度增长到len, 如果新的len比原来的大,
// 则将新增加的这部分字节置零.
sds sdsgrowzero(sds s, size_t len) {
    size_t curlen = sdslen(s);

    if (len <= curlen)
        return s;
    s = sdsMakeRoomFor(s, len - curlen);
    if (s == NULL)
        return NULL;

    /* Make sure added region doesn't contain garbage */
    // 将新增加的空间置零.
    memset(s + curlen, 0, (len - curlen + 1)); /* also set trailing \0 byte */
    sdssetlen(s, len);
    return s;
}

/* Append the specified binary-safe string pointed by 't' of 'len' bytes to the
 * end of the specified sds string 's'.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// sdscatlen 将从t起始的len个字节的数据追加到s末尾.
// 这个函数执行完后, 传入的s将不再有效(也不是绝对的,
// 比如说s原来的底层存储空间足够大).
sds sdscatlen(sds s, const void* t, size_t len) {
    size_t curlen = sdslen(s);

    s = sdsMakeRoomFor(s, len);
    if (s == NULL)
        return NULL;
    memcpy(s + curlen, t, len);
    sdssetlen(s, curlen + len);
    s[curlen + len] = '\0';
    return s;
}

/* Append the specified null termianted C string to the sds string 's'.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// sdscat 将C风格字符串追加到sds字符串s上.
// 这个函数执行完后, 传入的s将不再有效,
// 原来引用s的地方应该改为引用此函数的返回值
sds sdscat(sds s, const char* t) { return sdscatlen(s, t, strlen(t)); }

/* Append the specified sds 't' to the existing sds 's'.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// sdscatsds 将sds字符串t追加到sds字符串s上.
sds sdscatsds(sds s, const sds t) { return sdscatlen(s, t, sdslen(t)); }

/* Destructively modify the sds string 's' to hold the specified binary
 * safe string pointed by 't' of length 'len' bytes. */
// sdscpylen 将t起始地址的len个字节拷贝到sds字符串s中(即成为s的最新内容).
// 这个函数隐含: s与t不能重叠.
sds sdscpylen(sds s, const char* t, size_t len) {
    if (sdsalloc(s) < len) {
        s = sdsMakeRoomFor(s, len - sdslen(s));
        if (s == NULL)
            return NULL;
    }
    memcpy(s, t, len);
    s[len] = '\0';
    sdssetlen(s, len);
    return s;
}

/* Like sdscpylen() but 't' must be a null-termined string so that the length
 * of the string is obtained with strlen(). */
// sdscpy 将C风格字符串拷贝到s中(即成为s的最新内容).
sds sdscpy(sds s, const char* t) { return sdscpylen(s, t, strlen(t)); }

/* Helper for sdscatlonglong() doing the actual number -> string
 * conversion. 's' must point to a string with room for at least
 * SDS_LLSTR_SIZE bytes.
 *
 * The function returns the length of the null-terminated string
 * representation stored at 's'. */
// sdsll2str 这是一个辅助函数, 用于将value转换成对应的数值字符串形式,
// 并返回字符串长度. 这个函数要求s指向的底层存储空间至少有21个字节,
// 以便能够存储数值字符串形式.
#define SDS_LLSTR_SIZE 21
int sdsll2str(char* s, long long value) {
    char*              p;
    char               aux;
    unsigned long long v;
    size_t             l;

    /* Generate the string representation, this method produces
     * an reversed string. */
    // 下面语句将value转换成数值字符串形式(如果是负数还包含'-'),
    // 但是此字符串形式是逆序的.
    v = (value < 0) ? -value : value;
    p = s;
    do {
        *p++ = '0' + (v % 10);
        v /= 10;
    } while (v);
    if (value < 0)
        *p++ = '-';

    /* Compute length and add null term. */
    l  = p - s; // 字符串长度
    *p = '\0';

    /* Reverse the string. */
    // 转换成正常顺序, 进行首尾交换.
    p--;
    while (s < p) {
        aux = *s;
        *s  = *p;
        *p  = aux;
        s++;
        p--;
    }
    return l;
}

/* Identical sdsll2str(), but for unsigned long long type. */
// sdsull2str 将v转换成其对应的数值字符串形式, 并保存在s中.
// 同样这需要s指向的底层存储空间足够大(至少21字节).
int sdsull2str(char* s, unsigned long long v) {
    char*  p;
    char   aux;
    size_t l;

    /* Generate the string representation, this method produces
     * an reversed string. */
    p = s;
    do {
        *p++ = '0' + (v % 10);
        v /= 10;
    } while (v);

    /* Compute length and add null term. */
    l  = p - s;
    *p = '\0';

    /* Reverse the string. */
    p--;
    while (s < p) {
        aux = *s;
        *s  = *p;
        *p  = aux;
        s++;
        p--;
    }
    return l;
}

/* Create an sds string from a long long value. It is much faster than:
 *
 * sdscatprintf(sdsempty(),"%lld\n", value);
 */
// sdsfromlonglong 根据value创建一个sds(其是value的字符串形式)
sds sdsfromlonglong(long long value) {
    char buf[SDS_LLSTR_SIZE];
    int  len = sdsll2str(buf, value);

    return sdsnewlen(buf, len);
}

/* Like sdscatprintf() but gets va_list instead of being variadic. */
// sdscatvprintf 与函数sdscatprintf()功能类似, 只不过没有使用可变参数
sds sdscatvprintf(sds s, const char* fmt, va_list ap) {
    va_list cpy;
    char    staticbuf[1024];
    char*   buf = staticbuf;
    char*   t;
    size_t  buflen = strlen(fmt) * 2;

    /* We try to start using a static buffer for speed.
     * If not possible we revert to heap allocation. */
    if (buflen > sizeof(staticbuf)) {
        buf = s_malloc(buflen);
        if (buf == NULL)
            return NULL;
    } else {
        buflen = sizeof(staticbuf);
    }

    /* Try with buffers two times bigger every time we fail to
     * fit the string in the current buffer size. */
    while (1) {
        buf[buflen - 2] = '\0'; // 主要是用来检测buf空间是否足够
        va_copy(cpy, ap);
        vsnprintf(buf, buflen, fmt, cpy);
        va_end(cpy);
        if (buf[buflen - 2] != '\0') {
            // 现在buf指向的底层存储空间不足以保存格式化展开的字符串,
            // 需要重新申请大一倍的底层存储空间
            if (buf != staticbuf)
                s_free(buf);
            buflen *= 2;
            buf = s_malloc(buflen);
            if (buf == NULL)
                return NULL;
            continue;
        }
        break;
    }

    /* Finally concat the obtained string to the SDS string and return it. */
    t = sdscat(s, buf);
    // 动态内存一般原则是: 谁申请谁释放
    if (buf != staticbuf)
        s_free(buf);
    return t;
}

/* Append to the sds string 's' a string obtained using printf-alike format
 * specifier.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call.
 *
 * Example:
 *
 * s = sdsnew("Sum is: ");
 * s = sdscatprintf(s,"%d+%d = %d",a,b,a+b).
 *
 * Often you need to create a string from scratch with the printf-alike
 * format. When this is the need, just use sdsempty() as the target string:
 *
 * s = sdscatprintf(sdsempty(), "... your format ...", args);
 */
// sdscatprintf 将类似printf格式化字符串展开后, 追加后sds字符串s之后.
// 此函数执行后, s原来指向的存储空间将会变得无效(有时候有效,
// 但不应该依赖这种情况), 这也就意味着在执行函数之前, 引用s的地方都会变得无效.
sds sdscatprintf(sds s, const char* fmt, ...) {
    va_list ap;
    char*   t;
    va_start(ap, fmt);
    t = sdscatvprintf(s, fmt, ap);
    va_end(ap);
    return t;
}

/* This function is similar to sdscatprintf, but much faster as it does
 * not rely on sprintf() family functions implemented by the libc that
 * are often very slow. Moreover directly handling the sds string as
 * new data is concatenated provides a performance improvement.
 *
 * However this function only handles an incompatible subset of printf-alike
 * format specifiers:
 *
 * %s - C String
 * %S - SDS string
 * %i - signed int
 * %I - 64 bit signed integer (long long, int64_t)
 * %u - unsigned int
 * %U - 64 bit unsigned integer (unsigned long long, uint64_t)
 * %% - Verbatim "%" character.
 */
// sdscatfmt 功能与sdscatprintf类似,
// 但是此函数自己做格式化字符串解析而不依赖printf家族.
// 这个函数仅支持有限的格式化字符串解析.
// 进行sds格式化字符串展开时, 传入的参数s, 与变长参数中的某个参数不能重叠,
// 否则会引起不可预估的错误.
sds sdscatfmt(sds s, char const* fmt, ...) {
    size_t      initlen = sdslen(s);
    const char* f       = fmt;
    int         i;
    va_list     ap;

    va_start(ap, fmt);
    f = fmt;     /* Next format specifier byte to process. */
    i = initlen; /* Position of the next byte to write to dest str. */
    while (*f) {
        char               next;
        char*              str;
        size_t             l;
        long long          num;
        unsigned long long unum;

        /* Make sure there is always space for at least 1 char. */
        if (sdsavail(s) == 0) {
            s = sdsMakeRoomFor(s, 1);
        }

        switch (*f) {
        case '%':
            next = *(f + 1);
            f++;
            switch (next) {
            case 's':
            case 'S':
                str = va_arg(ap, char*);
                l   = (next == 's') ? strlen(str) : sdslen(str);
                if (sdsavail(s) < l) {
                    s = sdsMakeRoomFor(s, l);
                }
                memcpy(s + i, str, l);
                sdsinclen(s, l);
                i += l;
                break;
            case 'i':
            case 'I':
                if (next == 'i')
                    num = va_arg(ap, int);
                else
                    num = va_arg(ap, long long);
                {
                    char buf[SDS_LLSTR_SIZE];
                    l = sdsll2str(buf, num);
                    if (sdsavail(s) < l) {
                        s = sdsMakeRoomFor(s, l);
                    }
                    memcpy(s + i, buf, l);
                    sdsinclen(s, l);
                    i += l;
                }
                break;
            case 'u':
            case 'U':
                if (next == 'u')
                    unum = va_arg(ap, unsigned int);
                else
                    unum = va_arg(ap, unsigned long long);
                {
                    char buf[SDS_LLSTR_SIZE];
                    l = sdsull2str(buf, unum);
                    if (sdsavail(s) < l) {
                        s = sdsMakeRoomFor(s, l);
                    }
                    memcpy(s + i, buf, l);
                    sdsinclen(s, l);
                    i += l;
                }
                break;
            default: /* Handle %% and generally %<unknown>. */
                s[i++] = next;
                sdsinclen(s, 1);
                break;
            }
            break;
        default:
            s[i++] = *f;
            sdsinclen(s, 1);
            break;
        }
        f++;
    }
    va_end(ap);

    /* Add null-term */
    s[i] = '\0';
    return s;
}

/* Remove the part of the string from left and from right composed just of
 * contiguous characters found in 'cset', that is a null terminted C string.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call.
 *
 * Example:
 *
 * s = sdsnew("AA...AA.a.aa.aHelloWorld     :::");
 * s = sdstrim(s,"Aa. :");
 * printf("%s\n", s);
 *
 * Output will be just "Hello World".
 */
// sdstrim 将sds字符串两端连续出现在cset中的字符删除
sds sdstrim(sds s, const char* cset) {
    char*  start; // 指向sds字符串的第一个字符
    char*  end;   // 指向sds字符串的最后一个字符
    char*  sp;    // 从sds字符串第一个字符向最后一个字符移动
    char*  ep;    // 从sds字符串最后一个字符向第一个字符移动
    size_t len;

    sp = start = s;
    ep = end = s + sdslen(s) - 1;
    while (sp <= end && strchr(cset, *sp))
        sp++;
    while (ep > sp && strchr(cset, *ep))
        ep--;
    len = (sp > ep) ? 0 : ((ep - sp) + 1);
    if (s != sp)
        memmove(s, sp, len); // s与sp指向的底层存储空间存在重叠,
                             // 所以需要memmove而不能使用memcpy
    s[len] = '\0';
    sdssetlen(s, len);
    return s;
}

/* Turn the string into a smaller (or equal) string containing only the
 * substring specified by the 'start' and 'end' indexes.
 *
 * start and end can be negative, where -1 means the last character of the
 * string, -2 the penultimate character, and so forth.
 *
 * The interval is inclusive, so the start and end characters will be part
 * of the resulting string.
 *
 * The string is modified in-place.
 *
 * Example:
 *
 * s = sdsnew("Hello World");
 * sdsrange(s,1,-1); => "ello World"
 */
// sdsrange 将sds字符串变为sds字符串的子串, 子串为[start, end],
// start, end可以为负数; 负数意味着从尾部开始数.
// 当start在end后边时, 这个函数相当于清空内容, 效果与sdsclear()一样
void sdsrange(sds s, int start, int end) {
    size_t newlen;
    size_t len = sdslen(s);

    if (len == 0)
        return;
    if (start < 0) {
        start = len + start;
        if (start < 0)
            start = 0;
    }
    if (end < 0) {
        end = len + end;
        if (end < 0)
            end = 0;
    }
    newlen = (start > end) ? 0 : (end - start) + 1;
    if (newlen != 0) {
        if (start >= (signed)len) { // 这里的强制类型转换只是为了保险
            newlen = 0;
        } else if (end >= (signed)len) {
            end    = len - 1;
            newlen = (start > end) ? 0 : (end - start) + 1;
        }
    } else {
        start = 0;
    }
    if (start && newlen)
        memmove(s, s + start, newlen);
    s[newlen] = 0;
    sdssetlen(s, newlen);
}

/* Apply tolower() to every character of the sds string 's'. */
// sdstolower 将sds字符串转换成为小写
void sdstolower(sds s) {
    int len = sdslen(s);
    int j;

    for (j = 0; j < len; j++)
        s[j] = tolower(s[j]);
}

/* Apply toupper() to every character of the sds string 's'. */
// sdstoupper 将sds字符串转换成为大写
void sdstoupper(sds s) {
    int len = sdslen(s);
    int j;

    for (j = 0; j < len; j++)
        s[j] = toupper(s[j]);
}

/* Compare two sds strings s1 and s2 with memcmp().
 *
 * Return value:
 *
 *     positive if s1 > s2.
 *     negative if s1 < s2.
 *     0 if s1 and s2 are exactly the same binary string.
 *
 * If two strings share exactly the same prefix, but one of the two has
 * additional characters, the longer string is considered to be greater than
 * the smaller one. */
// sdscmp 使用memcmp()来比较sds字符串s1与s2
// 返回值:
//   如果 s1 > s2, 则返回正数
//   如果 s1 < s2, 则返回负数
//   如果 s1 == s2,则返回零
int sdscmp(const sds s1, const sds s2) {
    size_t l1;
    size_t l2;
    size_t minlen;
    int    cmp;

    l1     = sdslen(s1);
    l2     = sdslen(s2);
    minlen = (l1 < l2) ? l1 : l2;
    cmp    = memcmp(s1, s2, minlen);
    if (cmp == 0)
        return l1 - l2;
    return cmp;
}

/* Split 's' with separator in 'sep'. An array
 * of sds strings is returned. *count will be set
 * by reference to the number of tokens returned.
 *
 * On out of memory, zero length string, zero length
 * separator, NULL is returned.
 *
 * Note that 'sep' is able to split a string using
 * a multi-character separator. For example
 * sdssplit("foo_-_bar","_-_"); will return two
 * elements "foo" and "bar".
 *
 * This version of the function is binary-safe but
 * requires length arguments. sdssplit() is just the
 * same function but for zero-terminated strings.
 */
// sdssplitlen 使用字符串sep将字符串s分割. 返回一个指针,
// 指向分割生成的sds字符串数组的首元素, *count记录了返回了多少个sds字符串.
// 此函数返回会指针是使用malloc系列函数生成的, 故需要用户自己释放其底层内存.
sds* sdssplitlen(const char* s, int len, const char* sep, int seplen,
                 int* count) {
    int elements = 0; // 下一个需要产生的tokens在数组中的索引位置
    int  slots   = 5; // 返回的sds数组有多少个槽位置
    int  start   = 0; // 每个tokens的索引起始值
    int  j;           // 搜索的起始索引值
    sds* tokens;      // 需要返回的sds数组

    if (seplen < 1 || len < 0)
        return NULL;

    tokens = s_malloc(sizeof(sds) * slots);
    if (tokens == NULL)
        return NULL;

    if (len == 0) {
        *count = 0;
        return tokens;
    }
    // 分割字符串长度为seplen, 待分割的字符串长度为len,
    // 待分割的字符串中前len - (seplen - 1)个字符, 需要进行比较.
    // 原因是: 需要保证至少还能与一个分割字符串进行比较
    for (j = 0; j < (len - (seplen - 1)); j++) {
        /* make sure there is room for the next element and the final one */
        if (slots < elements + 2) {
            sds* newtokens;

            slots *= 2;
            newtokens = s_realloc(tokens, sizeof(sds) * slots);
            if (newtokens == NULL)
                goto cleanup;
            tokens = newtokens;
        }
        /* search the separator */
        // TODO: 为什么需要单独处理单字符的情况?
        if ((seplen == 1 && *(s + j) == sep[0]) ||
            (memcmp(s + j, sep, seplen) == 0)) {
            tokens[elements] = sdsnewlen(s + start, j - start);
            if (tokens[elements] == NULL)
                goto cleanup;
            elements++;
            start = j + seplen;
            // skip the separator 这里减1是因为for循环中j还进行了自增
            j = j + seplen - 1;
        }
    }
    /* Add the final element. We are sure there is room in the tokens array. */
    tokens[elements] = sdsnewlen(s + start, len - start);
    // sdsnewlen()分配内存失败, 则此函数执行失败, 需要清空分配的内存
    if (tokens[elements] == NULL)
        goto cleanup;
    elements++;
    *count = elements;
    return tokens;

    // cleanup: 用于释放返回的sds数组, 以及数组每个元素对应的sds字符串,
    // 以免内存泄露
cleanup : {
    int i;
    for (i = 0; i < elements; i++)
        sdsfree(tokens[i]);
    s_free(tokens);
    *count = 0;
    return NULL;
}
}

/* Free the result returned by sdssplitlen(), or do nothing if 'tokens' is NULL.
 */
// sdsfreesplitres 释放调用sdssplitlen()函数申请的底层内存空间
void sdsfreesplitres(sds* tokens, int count) {
    if (!tokens)
        return;
    while (count--)
        sdsfree(tokens[count]);
    s_free(tokens);
}

/* Append to the sds string "s" an escaped string representation where
 * all the non-printable characters (tested with isprint()) are turned into
 * escapes in the form "\n\r\a...." or "\x<hex-number>".
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// sdscatrepr 向sds字符串s中追加转义字符串,
// 追加的字符串中所有的不可打印字符都被转换成如下形式:
// "\n\r\a..."或"\x<hex-number>"
sds sdscatrepr(sds s, const char* p, size_t len) {
    s = sdscatlen(s, "\"", 1);
    while (len--) {
        switch (*p) {
        case '\\':
        case '"':
            s = sdscatprintf(s, "\\%c", *p);
            break;
        case '\n':
            s = sdscatlen(s, "\\n", 2);
            break;
        case '\r':
            s = sdscatlen(s, "\\r", 2);
            break;
        case '\t':
            s = sdscatlen(s, "\\t", 2);
            break;
        case '\a':
            s = sdscatlen(s, "\\a", 2);
            break;
        case '\b':
            s = sdscatlen(s, "\\b", 2);
            break;
        default:
            if (isprint(*p))
                s = sdscatprintf(s, "%c", *p);
            else
                s = sdscatprintf(s, "\\x%02x", (unsigned char)*p);
            break;
        }
        p++;
    }
    return sdscatlen(s, "\"", 1);
}

/* Helper function for sdssplitargs() that returns non zero if 'c'
 * is a valid hex digit. */
// is_hex_digit 检测c是否为十六进制使用的字符
int is_hex_digit(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

/* Helper function for sdssplitargs() that converts a hex digit into an
 * integer from 0 to 15 */
// hex_digit_to_int 将十六进制使用的字符转换成对应的整数
int hex_digit_to_int(char c) {
    switch (c) {
    case '0':
        return 0;
    case '1':
        return 1;
    case '2':
        return 2;
    case '3':
        return 3;
    case '4':
        return 4;
    case '5':
        return 5;
    case '6':
        return 6;
    case '7':
        return 7;
    case '8':
        return 8;
    case '9':
        return 9;
    case 'a':
    case 'A':
        return 10;
    case 'b':
    case 'B':
        return 11;
    case 'c':
    case 'C':
        return 12;
    case 'd':
    case 'D':
        return 13;
    case 'e':
    case 'E':
        return 14;
    case 'f':
    case 'F':
        return 15;
    default:
        return 0;
    }
}

/* Split a line into arguments, where every argument can be in the
 * following programming-language REPL-alike form:
 *
 * foo bar "newline are supported\n" and "\xff\x00otherstuff"
 *
 * The number of arguments is stored into *argc, and an array
 * of sds is returned.
 *
 * The caller should free the resulting array of sds strings with
 * sdsfreesplitres().
 *
 * Note that sdscatrepr() is able to convert back a string into
 * a quoted string in the same format sdssplitargs() is able to parse.
 *
 * The function returns the allocated tokens on success, even when the
 * input string is empty, or NULL if the input contains unbalanced
 * quotes or closed quotes followed by non space characters
 * as in: "foo"bar or "foo'
 */
// sdssplitargs 将一行字符串分割成参数
sds* sdssplitargs(const char* line, int* argc) {
    const char* p       = line;
    char*       current = NULL;
    char**      vector  = NULL;

    *argc = 0;
    while (1) {
        /* skip blanks */
        // 忽略开头的空白符, 并且要求p指向合法的字符
        while (*p && isspace(*p))
            p++;
        if (*p) {
            /* get a token */
            int inq  = 0; /* set to 1 if we are in "quotes" */
            int insq = 0; /* set to 1 if we are in 'single quotes' */
            int done = 0;

            if (current == NULL)
                current = sdsempty();
            while (!done) {
                if (inq) {
                    // 在双引号之中
                    if (*p == '\\' && *(p + 1) == 'x' &&
                        is_hex_digit(*(p + 2)) && is_hex_digit(*(p + 3))) {
                        // 类似这种形式的字符串: "\xab"
                        // 下面这段代码功能:
                        // 将符合上述格式的字符串转换成对应的字符
                        unsigned char byte;

                        byte = (hex_digit_to_int(*(p + 2)) * 16) +
                               hex_digit_to_int(*(p + 3));
                        current = sdscatlen(current, (char*)&byte, 1);
                        p += 3;
                    } else if (*p == '\\' && *(p + 1)) {
                        // 类似这种形式的字符串: "\r\n..."
                        // 下面这段代码功能:
                        // 将符合上述格式的字符串转换成对应的字符
                        char c;

                        p++;
                        switch (*p) {
                        case 'n':
                            c = '\n';
                            break;
                        case 'r':
                            c = '\r';
                            break;
                        case 't':
                            c = '\t';
                            break;
                        case 'b':
                            c = '\b';
                            break;
                        case 'a':
                            c = '\a';
                            break;
                        default:
                            c = *p; // 如"\m", 直接使用m
                            break;
                        }
                        current = sdscatlen(current, &c, 1);
                    } else if (*p == '"') {
                        /* closing quote must be followed by a space or
                         * nothing at all. */
                        // 右双引号后必须跟的是空白符或空字符
                        if (*(p + 1) && !isspace(*(p + 1)))
                            goto err;
                        done = 1;
                    } else if (!*p) {
                        /* unterminated quotes */
                        // 没有右双引号, 字符串就结束了
                        goto err;
                    } else {
                        // 双引号中的其他字符直接复制出来
                        current = sdscatlen(current, p, 1);
                    }
                } else if (insq) {
                    // 单引号之中
                    if (*p == '\\' && *(p + 1) == '\'') {
                        // '\''
                        // 解析转义的单引号
                        p++;
                        current = sdscatlen(current, "'", 1);
                    } else if (*p == '\'') {
                        /* closing quote must be followed by a space or
                         * nothing at all. */
                        // 右单引号后必须跟的是空白符或空字符
                        if (*(p + 1) && !isspace(*(p + 1)))
                            goto err;
                        done = 1;
                    } else if (!*p) {
                        /* unterminated quotes */
                        // 没有右单引号, 字符串就结束了
                        goto err;
                    } else {
                        // 单引号中的其他字符直接复制出来
                        current = sdscatlen(current, p, 1);
                    }
                } else {
                    switch (*p) {
                    case ' ':
                    case '\n':
                    case '\r':
                    case '\t':
                    case '\0':
                        // 一个token已经解析完成
                        done = 1;
                        break;
                    case '"':
                        inq = 1;
                        break;
                    case '\'':
                        insq = 1;
                        break;
                    default:
                        current = sdscatlen(current, p, 1);
                        break;
                    }
                }
                if (*p)
                    p++;
            }
            /* add the token to the vector */
            vector        = s_realloc(vector, ((*argc) + 1) * sizeof(char*));
            vector[*argc] = current;
            (*argc)++;
            current = NULL;
        } else {
            /* Even on empty input string return something not NULL. */
            // 如果输入的是空字符串, 仍然不直接返回NULL,
            // 而是申请了一个指针大小的堆内存, 是否会内存泄露?
            if (vector == NULL)
                vector = s_malloc(sizeof(void*));
            return vector;
        }
    }

err:
    while ((*argc)--)
        sdsfree(vector[*argc]);
    s_free(vector);
    if (current)
        sdsfree(current);
    *argc = 0;
    return NULL;
}

/* Modify the string substituting all the occurrences of the set of
 * characters specified in the 'from' string to the corresponding character
 * in the 'to' array.
 *
 * For instance: sdsmapchars(mystring, "ho", "01", 2)
 * will have the effect of turning the string "hello" into "0ell1".
 *
 * The function returns the sds string pointer, that is always the same
 * as the input pointer since no resize is needed. */
// sdsmapchars 将sds字符串中的, 出现在from集合的字符,
// 替换为to中对应的字符(from与to一一对应)
sds sdsmapchars(sds s, const char* from, const char* to, size_t setlen) {
    size_t j;
    size_t i;
    size_t l = sdslen(s);

    for (j = 0; j < l; j++) {
        for (i = 0; i < setlen; i++) {
            if (s[j] == from[i]) {
                s[j] = to[i];
                break;
            }
        }
    }
    return s;
}

/* Join an array of C strings using the specified separator (also a C string).
 * Returns the result as an sds string. */
// sdsjoin 将一组C风格字符串与间隔字符串拼接起来
sds sdsjoin(char** argv, int argc, char* sep) {
    sds join = sdsempty();
    int j;

    for (j = 0; j < argc; j++) {
        join = sdscat(join, argv[j]);
        if (j != argc - 1)
            join = sdscat(join, sep);
    }
    return join;
}

/* Like sdsjoin, but joins an array of SDS strings. */
// sdsjoinsds 将一组sds字符串与间隔字符串拼接起来
sds sdsjoinsds(sds* argv, int argc, const char* sep, size_t seplen) {
    sds join = sdsempty();
    int j;

    for (j = 0; j < argc; j++) {
        join = sdscatsds(join, argv[j]);
        if (j != argc - 1)
            join = sdscatlen(join, sep, seplen);
    }
    return join;
}

/* Wrappers to the allocators used by SDS. Note that SDS will actually
 * just use the macros defined into sdsalloc.h in order to avoid to pay
 * the overhead of function calls. Here we define these wrappers only for
 * the programs SDS is linked to, if they want to touch the SDS internals
 * even if they use a different allocator. */
void* sds_malloc(size_t size) { return s_malloc(size); }
void* sds_realloc(void* ptr, size_t size) { return s_realloc(ptr, size); }
void  sds_free(void* ptr) { s_free(ptr); }

#if defined(SDS_TEST_MAIN)
#include "limits.h"
#include "testhelp.h"
#include <stdio.h>

#define UNUSED(x) (void)(x)
int sdsTest(void) {
    {
        sds x = sdsnew("foo"), y;

        test_cond("Create a string and obtain the length",
                  sdslen(x) == 3 && memcmp(x, "foo\0", 4) == 0);
        sdsfree(x);

        x = sdsnewlen("foo", 2);
        test_cond("Create a string with specified length",
                  sdslen(x) == 2 && memcmp(x, "fo\0", 3) == 0);
        x = sdscat(x, "bar");

        test_cond("Strings concatenation",
                  sdslen(x) == 5 && memcmp(x, "fobar\0", 6) == 0);
        x = sdscpy(x, "a");

        test_cond("sdscpy() against an originally longer string",
                  sdslen(x) == 1 && memcmp(x, "a\0", 2) == 0);
        x = sdscpy(x, "xyzxxxxxxxxxxyyyyyyyyyykkkkkkkkkk");

        test_cond("sdscpy() against an originally shorter string",
                  sdslen(x) == 33 &&
                      memcmp(x, "xyzxxxxxxxxxxyyyyyyyyyykkkkkkkkkk\0", 33) ==
                          0);
        sdsfree(x);

        x = sdscatprintf(sdsempty(), "%d", 123);
        test_cond("sdscatprintf() seems working in the base case",
                  sdslen(x) == 3 && memcmp(x, "123\0", 4) == 0);
        sdsfree(x);

        x = sdsnew("--");
        x = sdscatfmt(x, "Hello %s World %I,%I--", "Hi!", LLONG_MIN, LLONG_MAX);
        test_cond("sdscatfmt() seems working in the base case",
                  sdslen(x) == 60 &&
                      memcmp(x,
                             "--Hello Hi! World -9223372036854775808,"
                             "9223372036854775807--",
                             60) == 0) printf("[%s]\n", x);
        sdsfree(x);

        x = sdsnew("--");
        x = sdscatfmt(x, "%u,%U--", UINT_MAX, ULLONG_MAX);
        test_cond("sdscatfmt() seems working with unsigned numbers",
                  sdslen(x) == 35 &&
                      memcmp(x, "--4294967295,18446744073709551615--", 35) ==
                          0);
        sdsfree(x);

        x = sdsnew(" x ");
        sdstrim(x, " x");
        test_cond("sdstrim() works when all chars match", sdslen(x) == 0);
        sdsfree(x);

        x = sdsnew(" x ");
        sdstrim(x, " ");
        test_cond("sdstrim() works when a single char remains",
                  sdslen(x) == 1 && x[0] == 'x');
        sdsfree(x);

        x = sdsnew("xxciaoyyy");
        sdstrim(x, "xy");
        test_cond("sdstrim() correctly trims characters",
                  sdslen(x) == 4 && memcmp(x, "ciao\0", 5) == 0);

        y = sdsdup(x);
        sdsrange(y, 1, 1);
        test_cond("sdsrange(...,1,1)",
                  sdslen(y) == 1 && memcmp(y, "i\0", 2) == 0);
        sdsfree(y);

        y = sdsdup(x);
        sdsrange(y, 1, -1);
        test_cond("sdsrange(...,1,-1)",
                  sdslen(y) == 3 && memcmp(y, "iao\0", 4) == 0);
        sdsfree(y);

        y = sdsdup(x);
        sdsrange(y, -2, -1);
        test_cond("sdsrange(...,-2,-1)",
                  sdslen(y) == 2 && memcmp(y, "ao\0", 3) == 0);
        sdsfree(y);

        y = sdsdup(x);
        sdsrange(y, 2, 1);
        test_cond("sdsrange(...,2,1)",
                  sdslen(y) == 0 && memcmp(y, "\0", 1) == 0);
        sdsfree(y);

        y = sdsdup(x);
        sdsrange(y, 1, 100);
        test_cond("sdsrange(...,1,100)",
                  sdslen(y) == 3 && memcmp(y, "iao\0", 4) == 0);
        sdsfree(y);

        y = sdsdup(x);
        sdsrange(y, 100, 100);
        test_cond("sdsrange(...,100,100)",
                  sdslen(y) == 0 && memcmp(y, "\0", 1) == 0);
        sdsfree(y);
        sdsfree(x);

        x = sdsnew("foo");
        y = sdsnew("foa");
        test_cond("sdscmp(foo,foa)", sdscmp(x, y) > 0);
        sdsfree(y);
        sdsfree(x);

        x = sdsnew("bar");
        y = sdsnew("bar");
        test_cond("sdscmp(bar,bar)", sdscmp(x, y) == 0);
        sdsfree(y);
        sdsfree(x);

        x = sdsnew("aar");
        y = sdsnew("bar");
        test_cond("sdscmp(bar,bar)", sdscmp(x, y) < 0);
        sdsfree(y);
        sdsfree(x);

        x = sdsnewlen("\a\n\0foo\r", 7);
        y = sdscatrepr(sdsempty(), x, sdslen(x));
        test_cond("sdscatrepr(...data...)",
                  memcmp(y, "\"\\a\\n\\x00foo\\r\"", 15) == 0);

        {
            unsigned int oldfree;
            char*        p;
            int          step = 10, j, i;

            sdsfree(x);
            sdsfree(y);
            x = sdsnew("0");
            test_cond("sdsnew() free/len buffers",
                      sdslen(x) == 1 && sdsavail(x) == 0);

            /* Run the test a few times in order to hit the first two
             * SDS header types. */
            for (i = 0; i < 10; i++) {
                int oldlen = sdslen(x);
                x          = sdsMakeRoomFor(x, step);
                int type   = x[-1] & SDS_TYPE_MASK;

                test_cond("sdsMakeRoomFor() len", sdslen(x) == oldlen);
                if (type != SDS_TYPE_5) {
                    test_cond("sdsMakeRoomFor() free", sdsavail(x) >= step);
                    oldfree = sdsavail(x);
                }
                p = x + oldlen;
                for (j = 0; j < step; j++) {
                    p[j] = 'A' + j;
                }
                sdsIncrLen(x, step);
            }
            test_cond(
                "sdsMakeRoomFor() content",
                memcmp("0ABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCD"
                       "EFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJ",
                       x, 101) == 0);
            test_cond("sdsMakeRoomFor() final length", sdslen(x) == 101);

            sdsfree(x);
        }
    }
    test_report();
    return 0;
}
#endif

#ifdef SDS_TEST_MAIN
int main(void) { return sdsTest(); }
#endif
