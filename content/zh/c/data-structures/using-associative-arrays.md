---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:45.792168-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C\u8BED\u8A00\u6CA1\u6709\u50CF\u4E00\
  \u4E9B\u9AD8\u7EA7\u8BED\u8A00\u90A3\u6837\u5185\u7F6E\u5BF9\u5173\u8054\u6570\u7EC4\
  \u7684\u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528\u7ED3\u6784\
  \u4F53\u548C\u6563\u5217\u6765\u6A21\u62DF\u5B83\u4EEC\u3002\u4EE5\u4E0B\u662F\u4E00\
  \u4E2A\u4F7F\u7528\u7ED3\u6784\u4F53\u548C\u7B80\u5355\u6563\u5217\u51FD\u6570\u7684\
  \u7B80\u5316\u793A\u4F8B\uFF0C\u7528\u4E8E\u5B9E\u73B0\u901A\u8FC7\u5B57\u7B26\u4E32\
  \u952E\u5B58\u50A8\u548C\u8BBF\u95EE\u6574\u6570\u7684\u5173\u8054\u6570\u7EC4\u3002\
  \ \u9996\u5148\uFF0C\u5B9A\u4E49\u4E00\u4E2A\u7ED3\u6784\u4F53\u6765\u8868\u793A\
  \u5355\u4E2A\u952E\u503C\u5BF9\uFF0C\u4EE5\u53CA\u53E6\u4E00\u4E2A\u7ED3\u6784\u4F53\
  \u6765\u8868\u793A\u5173\u8054\u6570\u7EC4\u672C\u8EAB\uFF1A."
lastmod: '2024-03-13T22:44:48.308307-06:00'
model: gpt-4-0125-preview
summary: "C\u8BED\u8A00\u6CA1\u6709\u50CF\u4E00\u4E9B\u9AD8\u7EA7\u8BED\u8A00\u90A3\
  \u6837\u5185\u7F6E\u5BF9\u5173\u8054\u6570\u7EC4\u7684\u652F\u6301\uFF0C\u4F46\u4F60\
  \u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528\u7ED3\u6784\u4F53\u548C\u6563\u5217\u6765\u6A21\
  \u62DF\u5B83\u4EEC\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u4F7F\u7528\u7ED3\u6784\u4F53\
  \u548C\u7B80\u5355\u6563\u5217\u51FD\u6570\u7684\u7B80\u5316\u793A\u4F8B\uFF0C\u7528\
  \u4E8E\u5B9E\u73B0\u901A\u8FC7\u5B57\u7B26\u4E32\u952E\u5B58\u50A8\u548C\u8BBF\u95EE\
  \u6574\u6570\u7684\u5173\u8054\u6570\u7EC4."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
