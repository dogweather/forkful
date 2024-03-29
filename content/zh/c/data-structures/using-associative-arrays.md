---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:45.792168-07:00
description: "\u5173\u8054\u6570\u7EC4\u5728\u5176\u4ED6\u8BED\u8A00\u4E2D\u88AB\u79F0\
  \u4E3A\u6620\u5C04\u6216\u5B57\u5178\uFF0C\u5B83\u4EEC\u662F\u7528\u4E8E\u9AD8\u6548\
  \u6570\u636E\u67E5\u627E\u548C\u64CD\u7EB5\u7684\u952E\u503C\u5BF9\u3002\u4E0E\u4F7F\
  \u7528\u6574\u6570\u7D22\u5F15\u7684\u4F20\u7EDF\u6570\u7EC4\u4E0D\u540C\uFF0C\u5173\
  \u8054\u6570\u7EC4\u4F7F\u7528\u952E\uFF0C\u8FD9\u4F7F\u5F97\u6570\u636E\u8BBF\u95EE\
  \u5BF9\u7A0B\u5E8F\u5458\u6765\u8BF4\u66F4\u76F4\u89C2\u3001\u66F4\u7075\u6D3B\u3002"
lastmod: '2024-03-13T22:44:48.308307-06:00'
model: gpt-4-0125-preview
summary: "\u5173\u8054\u6570\u7EC4\u5728\u5176\u4ED6\u8BED\u8A00\u4E2D\u88AB\u79F0\
  \u4E3A\u6620\u5C04\u6216\u5B57\u5178\uFF0C\u5B83\u4EEC\u662F\u7528\u4E8E\u9AD8\u6548\
  \u6570\u636E\u67E5\u627E\u548C\u64CD\u7EB5\u7684\u952E\u503C\u5BF9\u3002\u4E0E\u4F7F\
  \u7528\u6574\u6570\u7D22\u5F15\u7684\u4F20\u7EDF\u6570\u7EC4\u4E0D\u540C\uFF0C\u5173\
  \u8054\u6570\u7EC4\u4F7F\u7528\u952E\uFF0C\u8FD9\u4F7F\u5F97\u6570\u636E\u8BBF\u95EE\
  \u5BF9\u7A0B\u5E8F\u5458\u6765\u8BF4\u66F4\u76F4\u89C2\u3001\u66F4\u7075\u6D3B\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组在其他语言中被称为映射或字典，它们是用于高效数据查找和操纵的键值对。与使用整数索引的传统数组不同，关联数组使用键，这使得数据访问对程序员来说更直观、更灵活。

## 如何操作：

C语言没有像一些高级语言那样内置对关联数组的支持，但你可以通过使用结构体和散列来模拟它们。以下是一个使用结构体和简单散列函数的简化示例，用于实现通过字符串键存储和访问整数的关联数组。

首先，定义一个结构体来表示单个键值对，以及另一个结构体来表示关联数组本身：

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} 键值对;

typedef struct {
    键值对* items[TABLE_SIZE];
} 关联数组;

unsigned int 散列(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(关联数组* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void 插入(关联数组* array, char* key, int value) {
    unsigned int 槽 = 散列(key);

    键值对* item = (键值对*)malloc(sizeof(键值对));
    item->key = strdup(key);
    item->value = value;

    array->items[槽] = item;
}

int 查找(关联数组* array, char* key) {
    unsigned int 槽 = 散列(key);

    if (array->items[槽]) {
        return array->items[槽]->value;
    }
    return -1;
}

int main() {
    关联数组 a;
    initArray(&a);

    插入(&a, "key1", 1);
    插入(&a, "key2", 2);

    printf("%d\n", 查找(&a, "key1")); // 输出：1
    printf("%d\n", 查找(&a, "key2")); // 输出：2

    return 0;
}
```

此示例演示了基本操作：初始化关联数组、插入键值对、通过键查找值。请注意，此代码缺少冲突处理，仅用于教育目的。

## 深入探讨

关联数组的概念早于C语言，但由于该语言的低级性质，并不直接支持它们作为内置类型。这鼓励对数据结构和算法有更深入的理解，包括用于高效键值映射的散列机制。许多C库和框架提供了实现关联数组的更复杂方法，例如GLib的`GHashTable`，它提供了一个健壮的实现，包括冲突处理、动态调整大小以及对任意键和值类型的支持。

虽然在C中手动构建关联数组与拥有内置支持的语言相比可能看起来比较繁琐，但它为理解数据结构的内部工作提供了宝贵的见解，提高了程序员在问题解决和优化方面的技能。然而，对于生产代码或更复杂的应用程序，利用像GLib这样的现有库通常是一种更实际和节省时间的方法。
