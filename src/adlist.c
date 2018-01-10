/* adlist.c - A generic doubly linked list implementation
 *
 * Copyright (c) 2006-2010, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include <stdlib.h>

#include "adlist.h"
#include "zmalloc.h"

/* Create a new list. The created list can be freed with
 * AlFreeList(), but private value of every node need to be freed
 * by the user before to call AlFreeList().
 *
 * On error, NULL is returned. Otherwise the pointer to the new list. */
// listCreate 创建一个双端链表
// 此函数创建的双端链表可以被AlFreeList()函数来释放,
// 但是双端链表中每个元素对应的数据,
// 需要用户自己在使用AlFreeList()释放链表之前释放数据占用的内存.
//
// 出错时返回NULL, 成功则返回新链表的元数据首地址.
list* listCreate(void) {
    struct list* list;

    if ((list = zmalloc(sizeof(*list))) == NULL)
        return NULL;
    list->head  = NULL;
    list->tail  = NULL;
    list->len   = 0;
    list->dup   = NULL;
    list->free  = NULL;
    list->match = NULL;
    return list;
}

/* Remove all the elements from the list without destroying the list itself. */
// listEmpty
// 释放双端链表list各元素本身数据部分所使用的内存空间和各元素本身所占用的内存空间,
// 但是list元数据所占的内存空间不释放.
void listEmpty(list* list) {
    unsigned long len;
    listNode*     current;
    listNode*     next;

    current = list->head;
    len     = list->len;
    while (len--) {
        next = current->next;
        if (list->free)
            list->free(current->value);
        zfree(current);
        current = next;
    }
    list->head = list->tail = NULL;
    list->len               = 0;
}

/* Free the whole list.
 *
 * This function can't fail. */
// listRelease 销毁双端链表list, 释放整个list占用的内存空间:
//     (1) 数据部分占用的内存空间
//     (2) 各元素占用的内存空间
//     (3) list元数据占用的内存空间
// 这个函数失败的话会产生内存泄露
void listRelease(list* list) {
    listEmpty(list);
    zfree(list);
}

/* Add a new node to the list, to head, containing the specified 'value'
 * pointer as value.
 *
 * On error, NULL is returned and no operation is performed (i.e. the
 * list remains unaltered).
 * On success the 'list' pointer you pass to the function is returned. */
// listAddNodeHead 使用头插法, 将用户提供的数据添加到双端链表list的头元素之前.
//
// 此函数成功执行, 则返回list指针; 失败, 则返回NULL, 并且不对list作修改
list* listAddNodeHead(list* list, void* value) {
    listNode* node;

    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    node->value = value;
    if (list->len == 0) {
        list->head = list->tail = node;
        node->prev = node->next = NULL;
    } else {
        node->prev       = NULL;
        node->next       = list->head;
        list->head->prev = node;
        list->head       = node;
    }
    list->len++;
    return list;

    /* My personal implementation
    listNode* node;

    if ((node = zmalloc(sizeof(listNode))) == NULL) {
        return NULL;
    }

    node->value = value;
    node->prev  = NULL;
    node->next  = NULL;

    if (list->len == 0) {
        list->head = node;
        list->tail = node;
    } else {
        node->next       = list->head;
        list->head->prev = node;
        list->head       = node;
    }
    list->len++;
    return list;
    */
}

/* Add a new node to the list, to tail, containing the specified 'value'
 * pointer as value.
 *
 * On error, NULL is returned and no operation is performed (i.e. the
 * list remains unaltered).
 * On success the 'list' pointer you pass to the function is returned. */
// listAddNodeTail 使用尾插法, 将用户提供的数据添加到双端链表list的尾元素之后
//
// 此函数成功执行, 则返回list指针; 失败, 则返回NULL, 并且不对list作修改
list* listAddNodeTail(list* list, void* value) {
    listNode* node;

    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    node->value = value;
    if (list->len == 0) {
        list->head = list->tail = node;
        node->prev = node->next = NULL;
    } else {
        node->prev       = list->tail;
        node->next       = NULL;
        list->tail->next = node;
        list->tail       = node;
    }
    list->len++;
    return list;
}

// listInsertNode 使用用户提供的数据value生成一个双端链表元素, 根据after的指示,
// 将新生成的元素插入到old_node之前还是之后
//
// 此函数成功执行, 则返回list指针; 失败, 则返回NULL, 并且不对list作修改
list* listInsertNode(list* list, listNode* old_node, void* value, int after) {
    listNode* node;

    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    node->value = value;
    if (after) {
        node->prev = old_node;
        node->next = old_node->next;
        if (list->tail == old_node) {
            list->tail = node;
        }
    } else {
        node->next = old_node;
        node->prev = old_node->prev;
        if (list->head == old_node) {
            list->head = node;
        }
    }
    if (node->prev != NULL) {
        node->prev->next = node;
    }
    if (node->next != NULL) {
        node->next->prev = node;
    }
    list->len++;
    return list;
}

/* Remove the specified node from the specified list.
 * It's up to the caller to free the private value of the node.
 *
 * This function can't fail. */
// listDelNode 从指定的双端链表list中删除指定的node节点,
// 在双端链表list提供释放函数的前提下, 此函数会释放数据占用的内存.
void listDelNode(list* list, listNode* node) {
    if (node->prev)
        node->prev->next = node->next;
    else
        list->head = node->next;
    if (node->next)
        node->next->prev = node->prev;
    else
        list->tail = node->prev;
    if (list->free)
        list->free(node->value);
    zfree(node);
    list->len--;
}

/* Returns a list iterator 'iter'. After the initialization every
 * call to listNext() will return the next element of the list.
 *
 * This function can't fail. */
// listGetIterator 生成并返回双端链表list的迭代器
// direction用于指示迭代器迭代的方向
//     0: head -> tail
//     1: tail -> head
// listNext()用此函数返回的迭代器返回list下一个待处理的链表元素
listIter* listGetIterator(list* list, int direction) {
    listIter* iter;

    if ((iter = zmalloc(sizeof(*iter))) == NULL)
        return NULL;
    if (direction == AL_START_HEAD)
        iter->next = list->head;
    else
        iter->next = list->tail;
    iter->direction = direction;
    return iter;
}

/* Release the iterator memory */
// listReleaseIterator 释放由listGetIterator()生成的迭代器所占用的内存
void listReleaseIterator(listIter* iter) { zfree(iter); }

/* Create an iterator in the list private iterator structure */
// listRewind 将双端链表list的迭代器li, 重新倒回list的开头
void listRewind(list* list, listIter* li) {
    li->next      = list->head;
    li->direction = AL_START_HEAD;
}

// listRewindTail 将双端链表list的迭代器li(在此, 逻辑上可以认为是逆向迭代器),
// 重新倒回list的结尾
void listRewindTail(list* list, listIter* li) {
    li->next      = list->tail;
    li->direction = AL_START_TAIL;
}

/* Return the next element of an iterator.
 * It's valid to remove the currently returned element using
 * listDelNode(), but not to remove other elements.
 *
 * The function returns a pointer to the next element of the list,
 * or NULL if there are no more elements, so the classical usage patter
 * is:
 *
 * iter = listGetIterator(list,<direction>);
 * while ((node = listNext(iter)) != NULL) {
 *     doSomethingWith(listNodeValue(node));
 * }
 *
 * */
// listNext 返回当前迭代器iter所代表的元素,
// 并将迭代器向前迭代一次(这里的向前可以是: head -> tail, tail -> head)
// 迭代完成时, 返回NULL.
listNode* listNext(listIter* iter) {
    listNode* current = iter->next;

    if (current != NULL) {
        if (iter->direction == AL_START_HEAD)
            iter->next = current->next;
        else
            iter->next = current->prev;
    }
    return current;
}

/* Duplicate the whole list. On out of memory NULL is returned.
 * On success a copy of the original list is returned.
 *
 * The 'Dup' method set with listSetDupMethod() function is used
 * to copy the node value. Otherwise the same pointer value of
 * the original node is used as value of the copied node.
 *
 * The original list both on success or error is never modified. */
// listDup 完整拷贝整个双端链表orig. 如果内存不足, 则返回NULL.
// 此函数成功执行, 则返回拷贝生成的新链表元数据的指针.
//
// 使用listSetDupMethod()方法为双端链表设置的dup方法是用来拷贝链表元素的数据部分的.
// 如果链表的dup不为NULL, 则使用此方法来拷贝链表元素的数据部分; 否则,
// 新创建的拷贝链表与原链表共享链表元素的数据部分
//
// 此函数执行无论成功还是失败, 都不会影响原链表orig
list* listDup(list* orig) {
    list*     copy;
    listIter  iter;
    listNode* node;

    if ((copy = listCreate()) == NULL)
        return NULL;
    copy->dup   = orig->dup;
    copy->free  = orig->free;
    copy->match = orig->match;
    listRewind(orig, &iter);
    while ((node = listNext(&iter)) != NULL) {
        void* value;

        if (copy->dup) {
            value = copy->dup(node->value);
            if (value == NULL) {
                listRelease(copy);
                return NULL;
            }
        } else
            value = node->value;
        if (listAddNodeTail(copy, value) == NULL) {
            listRelease(copy);
            return NULL;
        }
    }
    return copy;
}

/* Search the list for a node matching a given key.
 * The match is performed using the 'match' method
 * set with listSetMatchMethod(). If no 'match' method
 * is set, the 'value' pointer of every node is directly
 * compared with the 'key' pointer.
 *
 * On success the first matching node pointer is returned
 * (search starts from head). If no matching node exists
 * NULL is returned. */
// listSearchKey 返回双端链表list中指定key的链表元素.
// 此函数的搜索过程分为两种情况
//     (1)list有match函数,
//     则直接使用list的match函数(match函数通过listSetMatchMethod()设置)
//     (2)list没有match函数, 则直接比较链表元素数据部分指针与key是否相等
//
// 搜索成功则返回第一个相等的元素, 否则, 返回NULL.
listNode* listSearchKey(list* list, void* key) {
    listIter  iter;
    listNode* node;

    listRewind(list, &iter);
    while ((node = listNext(&iter)) != NULL) {
        if (list->match) {
            if (list->match(node->value, key)) {
                return node;
            }
        } else {
            if (key == node->value) {
                return node;
            }
        }
    }
    return NULL;
}

/* Return the element at the specified zero-based index
 * where 0 is the head, 1 is the element next to head
 * and so on. Negative integers are used in order to count
 * from the tail, -1 is the last element, -2 the penultimate
 * and so on. If the index is out of range NULL is returned. */
// listIndex 返回双端链表list中指定索引的链表元素
// 索引值从0开始, 可以为负数; -1表示倒数第一个链表元素, -2为倒数第二个链表元素.
listNode* listIndex(list* list, long index) {
    listNode* n;

    if (index < 0) {
        index = (-index) - 1;
        n     = list->tail;
        while (index-- && n)
            n = n->prev;
    } else {
        n = list->head;
        while (index-- && n)
            n = n->next;
    }
    return n;
}

/* Rotate the list removing the tail node and inserting it to the head. */
// listRotate 旋转双端链表list, 将尾节点删除并插入到链表头部
void listRotate(list* list) {
    listNode* tail = list->tail;

    if (listLength(list) <= 1)
        return;

    /* Detach current tail */
    list->tail       = tail->prev;
    list->tail->next = NULL;
    /* Move it as head */
    list->head->prev = tail;
    tail->prev       = NULL;
    tail->next       = list->head;
    list->head       = tail;
}

/* Add all the elements of the list 'o' at the end of the
 * list 'l'. The list 'other' remains empty but otherwise valid. */
// listJoin 将双端链表o中的链表元素追加到双端链表l尾部, 并将链表o置空
void listJoin(list* l, list* o) {
    if (o->head)
        o->head->prev = l->tail;

    if (l->tail)
        l->tail->next = o->head;
    else
        l->head = o->head;

    if (o->tail)
        l->tail = o->tail;
    l->len += o->len;

    /* Setup other as an empty list. */
    o->head = o->tail = NULL;
    o->len            = 0;
}
