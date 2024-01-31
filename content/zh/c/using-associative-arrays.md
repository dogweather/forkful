---
title:                "使用关联数组"
date:                  2024-01-30T19:10:08.503157-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组或哈希映射是允许你使用键存储和检索数据的键-值对。它们在C语言中非常有用，因为与列表相比，它们使数据访问速度更快，特别是当你处理大量数据时。

## 如何操作：

C语言没有像某些其他语言那样对关联数组的内置支持，但我们可以使用结构体和一些库函数来获得类似的功能。这里有一个使用`uthash`库的简单实现，你需要将其包含在你的项目中。

首先，定义一个结构体来保存你的键-值对：

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // 这将是我们的键
    char name[10]; // 这是与我们的键关联的值
    UT_hash_handle hh; // 使这个结构体可哈希化
} person;
```

接下来，让我们添加一些条目并检索它们：

```C
int main() {
    person *my_people = NULL, *s;

    // 添加一个条目
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // 检索一个条目
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    if (s) {
        printf("已找到: %s\n", s->name);
    }
    
    return 0;
}
```

样本输出将是：

```
已找到: Alice
```

完成后不要忘记释放分配的内存和释放哈希表以避免内存泄露。

## 深入探讨

虽然关联数组不是C语言的原生功能，但像`uthash`这样的库很好地填补了这一空白，提供了一种相当直接的方式来使用这一功能。从历史上看，C语言开发人员不得不实现他们版本的这些数据结构，导致各种复杂的实现，特别是对于那些刚开始使用该语言的人来说。

记住，使用C语言中的关联数组的效率很大程度上取决于哈希函数如何将值分布在表中以最小化冲突。虽然像`uthash`这样的库提供了使用上的便利与性能之间的良好平衡，在性能至关重要的关键应用程序中，你可能想要定制或实现自己的哈希表。

对于需要最高效率的应用程序，使用替代数据结构或甚至其他具有内置关联数组支持的编程语言可能是更好的选择。然而，在许多情况下，特别是当你已经在C环境中工作时，使用像`uthash`这样的库在性能和便利性之间提供了一个实用的平衡。
