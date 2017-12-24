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

#ifndef __SDS_H
#define __SDS_H

#define SDS_MAX_PREALLOC (1024 * 1024)

#include <stdarg.h>
#include <stdint.h>
#include <sys/types.h>

// simple dynamic string类型
typedef char* sds;

// __attribute__((__packed__)) gcc扩展属性 执行1字节对齐

/* Note: sdshdr5 is never used, we just access the flags byte directly.
 * However is here to document the layout of type 5 SDS strings. */
struct __attribute__((__packed__)) sdshdr5 {
    unsigned char flags; /* 3 lsb of type, and 5 msb of string length */
    char          buf[];
};

// len: 字符串长度
// alloc: cap 底层存储空间(连续)的大小
// flags: sds首部标识, 1个字节长度, 低3位为sds类型, 高5位暂未使用
struct __attribute__((__packed__)) sdshdr8 {
    uint8_t       len;   /* used */
    uint8_t       alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char          buf[];
};
struct __attribute__((__packed__)) sdshdr16 {
    uint16_t      len;   /* used */
    uint16_t      alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char          buf[];
};
struct __attribute__((__packed__)) sdshdr32 {
    uint32_t      len;   /* used */
    uint32_t      alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char          buf[];
};
struct __attribute__((__packed__)) sdshdr64 {
    uint64_t      len;   /* used */
    uint64_t      alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char          buf[];
};

#define SDS_TYPE_5 0    // 第一种sds类型, 未使用
#define SDS_TYPE_8 1    // 第二种sds类型, 使用1个字节表示大小
#define SDS_TYPE_16 2   // 第三种sds类型, 使用2个字节表示大小
#define SDS_TYPE_32 3   // 第四种sds类型, 使用4个字节表示大小
#define SDS_TYPE_64 4   // 第五种sds类型, 使用8个字节表示大小
#define SDS_TYPE_MASK 7 // 二进制111, 用于提取低三位
#define SDS_TYPE_BITS 3 // 表示sds类型的二进制位数

// T: 8, 16, 32, 64四个数之一
// s: 一个地址, 指向sds的首地址
// (s隐含的类型要求是: void* 或 char* 或 unsigned char*)
// 此宏用于定义一个指向sds首部的指针
#define SDS_HDR_VAR(T, s)                                                      \
    struct sdshdr##T* sh =                                                     \
        (struct sdshdr##T*)((s) - (sizeof(struct sdshdr##T)));

// T: 8, 16, 32, 64四个数之一
// s: 一个地址, 指向sds的首地址
// (s隐含的类型要求是: void* 或 char* 或 unsigned char*)
// 此宏用于计算sds的首部地址
#define SDS_HDR(T, s) ((struct sdshdr##T*)((s) - (sizeof(struct sdshdr##T))))
#define SDS_TYPE_5_LEN(f) ((f) >> SDS_TYPE_BITS)

// sdslen 返回s的长度
static inline size_t sdslen(const sds s) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5:
        return SDS_TYPE_5_LEN(flags);
    case SDS_TYPE_8:
        return SDS_HDR(8, s)->len;
    case SDS_TYPE_16:
        return SDS_HDR(16, s)->len;
    case SDS_TYPE_32:
        return SDS_HDR(32, s)->len;
    case SDS_TYPE_64:
        return SDS_HDR(64, s)->len;
    }
    return 0;
}

// sdsavail 返回s底层存储空间还有多少空余
static inline size_t sdsavail(const sds s) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5: {
        return 0;
    }
    case SDS_TYPE_8: {
        SDS_HDR_VAR(8, s);
        return sh->alloc - sh->len;
    }
    case SDS_TYPE_16: {
        SDS_HDR_VAR(16, s);
        return sh->alloc - sh->len;
    }
    case SDS_TYPE_32: {
        SDS_HDR_VAR(32, s);
        return sh->alloc - sh->len;
    }
    case SDS_TYPE_64: {
        SDS_HDR_VAR(64, s);
        return sh->alloc - sh->len;
    }
    }
    return 0;
}

// sdssetlen 设置s的len为newlen
static inline void sdssetlen(sds s, size_t newlen) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5: {
        unsigned char* fp = ((unsigned char*)s) - 1;
        *fp               = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
    } break;
    case SDS_TYPE_8:
        SDS_HDR(8, s)->len = newlen;
        break;
    case SDS_TYPE_16:
        SDS_HDR(16, s)->len = newlen;
        break;
    case SDS_TYPE_32:
        SDS_HDR(32, s)->len = newlen;
        break;
    case SDS_TYPE_64:
        SDS_HDR(64, s)->len = newlen;
        break;
    }
}

// sdsinclen 为s的len增加inc
static inline void sdsinclen(sds s, size_t inc) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5: {
        unsigned char* fp     = ((unsigned char*)s) - 1;
        unsigned char  newlen = SDS_TYPE_5_LEN(flags) + inc;
        *fp                   = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
    } break;
    case SDS_TYPE_8:
        SDS_HDR(8, s)->len += inc;
        break;
    case SDS_TYPE_16:
        SDS_HDR(16, s)->len += inc;
        break;
    case SDS_TYPE_32:
        SDS_HDR(32, s)->len += inc;
        break;
    case SDS_TYPE_64:
        SDS_HDR(64, s)->len += inc;
        break;
    }
}

/* sdsalloc() = sdsavail() + sdslen() */
// sdsalloc 返回s底层存储空间总的大小
static inline size_t sdsalloc(const sds s) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5:
        return SDS_TYPE_5_LEN(flags);
    case SDS_TYPE_8:
        return SDS_HDR(8, s)->alloc;
    case SDS_TYPE_16:
        return SDS_HDR(16, s)->alloc;
    case SDS_TYPE_32:
        return SDS_HDR(32, s)->alloc;
    case SDS_TYPE_64:
        return SDS_HDR(64, s)->alloc;
    }
    return 0;
}

// sdssetalloc 设置s的alloc, 即底层存储空间的大小
static inline void sdssetalloc(sds s, size_t newlen) {
    unsigned char flags = s[-1];
    switch (flags & SDS_TYPE_MASK) {
    case SDS_TYPE_5:
        /* Nothing to do, this type has no total allocation info. */
        break;
    case SDS_TYPE_8:
        SDS_HDR(8, s)->alloc = newlen;
        break;
    case SDS_TYPE_16:
        SDS_HDR(16, s)->alloc = newlen;
        break;
    case SDS_TYPE_32:
        SDS_HDR(32, s)->alloc = newlen;
        break;
    case SDS_TYPE_64:
        SDS_HDR(64, s)->alloc = newlen;
        break;
    }
}

sds  sdsnewlen(const void* init, size_t initlen);
sds  sdsnew(const char* init);
sds  sdsempty(void);
sds  sdsdup(const sds s);
void sdsfree(sds s);
sds  sdsgrowzero(sds s, size_t len);
sds  sdscatlen(sds s, const void* t, size_t len);
sds  sdscat(sds s, const char* t);
sds  sdscatsds(sds s, const sds t);
sds  sdscpylen(sds s, const char* t, size_t len);
sds  sdscpy(sds s, const char* t);

// GCC __attribute__扩展
// __attribute__((format(archetype, string-index, first-to-check)))
// format属性指定一个函数签名使用类似printf, scanf, strftime, 或者strfmon的风格,
// 使GCC做相应的函数签名检查.
// archetype:
//     指定format属性如何解释, 它可以是printf, scanf, strftime,
//     gnu_printf, gnu_scanf, gnu_strftime, strfmon. gnu开头的优先使用GNU C
//     Library接受的格式, 而其他的优先使用系统C运行库接受的格式.
// string-index:
//     指定哪个参数是格式化字符串(fmt), 其中索引从1开始,
//     而不是从0开始.
// first-to-check:
//     指定与格式化字符串(fmt)相对应的第一个参数的索引值.
//     对于有些没有需要检测参数的函数来说(如vprintf), 这个值为0. 也就是说,
//     参数不是'...'的, 指定为0. 对于strftime风格来说, 这个值需要指定为0.
// 由于C++的非静态方法中隐含了一个this参数,
// 所以这些参数的索引值需要从2开始而不是从1开始. 也就是说, string-index,
// first-to-check的值从2开始计算.
// 如: sds sdscatprintf(sds s, const char* fmt, ...)
//     __attribute__((format(printf, 2, 3)));
// 这会使GCC检查sdscatprintf的参数风格是否与printf函数的参数风格一致.
// 格式化字符串(fmt)是sdscatprintf函数的第二个参数,
// GCC从sdscatprintf函数的第三个参数开始做检测.

sds sdscatvprintf(sds s, const char* fmt, va_list ap);
#ifdef __GNUC__
sds sdscatprintf(sds s, const char* fmt, ...)
    __attribute__((format(printf, 2, 3)));
#else
sds sdscatprintf(sds s, const char* fmt, ...);
#endif

sds  sdscatfmt(sds s, char const* fmt, ...);
sds  sdstrim(sds s, const char* cset);
void sdsrange(sds s, int start, int end);
void sdsupdatelen(sds s);
void sdsclear(sds s);
int  sdscmp(const sds s1, const sds s2);
sds* sdssplitlen(const char* s, int len, const char* sep, int seplen,
                 int* count);
void sdsfreesplitres(sds* tokens, int count);
void sdstolower(sds s);
void sdstoupper(sds s);
sds  sdsfromlonglong(long long value);
sds  sdscatrepr(sds s, const char* p, size_t len);
sds* sdssplitargs(const char* line, int* argc);
sds  sdsmapchars(sds s, const char* from, const char* to, size_t setlen);
sds  sdsjoin(char** argv, int argc, char* sep);
sds  sdsjoinsds(sds* argv, int argc, const char* sep, size_t seplen);

/* Low level functions exposed to the user API */
sds    sdsMakeRoomFor(sds s, size_t addlen);
void   sdsIncrLen(sds s, int incr);
sds    sdsRemoveFreeSpace(sds s);
size_t sdsAllocSize(sds s);
void*  sdsAllocPtr(sds s);

/* Export the allocator used by SDS to the program using SDS.
 * Sometimes the program SDS is linked to, may use a different set of
 * allocators, but may want to allocate or free things that SDS will
 * respectively free or allocate. */
void* sds_malloc(size_t size);
void* sds_realloc(void* ptr, size_t size);
void  sds_free(void* ptr);

#ifdef REDIS_TEST
int sdsTest(int argc, char* argv[]);
#endif

#endif
