---
title:    "C: 连接字符串"
keywords: ["C"]
---

{{< edit_this_page >}}

为什么：有至1-2句话阐述为什么人们会使用字符串连接。

## 为什么

在编程中，经常会遇到需要将多个字符串结合在一起的情况。例如，在字符串拼接时，我们可能需要将一个人的名字和姓氏结合起来，或者将多条文本消息合并成一条长文本。使用字符串连接能够有效地帮助我们实现这些需求。

## 如何实现

要进行字符串连接，我们需要使用C语言中的一个函数`strcat()`。这个函数的功能是将一个字符串追加在另一个字符串的末尾，从而形成一个新的字符串。示例如下：

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    // 创建两个字符串
    char first_name[20] = "张";
    char last_name[20] = "三";

    // 使用strcat()函数将两个字符串连接
    strcat(first_name, last_name);

    // 输出结果
    printf("%s\n", first_name);

    return 0;
}
```

输出结果为`张三`。我们可以看到，`strcat()`函数将`last_name`字符串连接到了`first_name`字符串的末尾，从而形成了新的字符串。

除了`strcat()`函数外，我们还可以使用另一个函数`strncat()`来实现字符串连接。与`strcat()`不同的是，`strncat()`需要指定连接的字符个数，从而避免内存溢出的情况。示例如下：

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    // 创建两个字符串
    char first_name[20] = "张";
    char last_name[20] = "三";

    // 使用strncat()函数将两个字符串连接
    strncat(first_name, last_name, 1); // 连接一个字符

    // 输出结果
    printf("%s\n", first_name);

    return 0;
}
```

输出结果为`张三`。我们可以看到，通过指定连接的字符个数，我们只连接了`last_name`字符串的第一个字符，从而得到了`张三`这个结果。

## 深入了解

在C语言中，字符串被视为字符数组，因此我们可以使用循环来遍历字符串中的每一个字符，从而实现更灵活的字符串连接操作。同时，我们也可以使用`strcmp()`函数来比较两个字符串，从而确定其连接顺序。值得注意的是，在使用`strcat()`和`strncat()`进行字符串连接时，我们需要保证目标字符串的长度足够长，以免造成内存溢出的情况。

## 另请参阅

- [C语言中的字符串操作](https://www.geeksforgeeks.org/string-handling-strings-in-c/)
- [C语言字符串连接函数strcat()的使用](https://www.runoob.com/cprogramming/c-function-strcat.html)
- [C语言字符串比较函数strcmp()的使用](https://www.runoob.com/cprogramming/c-function-strcmp.html)