---
title:                "C: 改变字符串的大小写"
simple_title:         "改变字符串的大小写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在程序设计中，有时候我们需要将字符串中的字母变成大写。这可能是为了遵循特定的格式要求，或者是为了方便数据的处理。无论出于什么原因，大写化字符串是一个常见的需求，因此学习如何实现这一功能是很重要的。

## 如何实现

大写化字符串可以通过使用C语言的标准库函数 `toupper()`来实现。这个函数可以将小写字母转换为大写字母，并返回转换后的字符。下面的代码展示了如何使用 `toupper()`函数来大写化一个字符串，并打印出结果：

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // 声明一个字符串并赋值
    char str[] = "hello world";
    // 用于遍历字符串的循环
    int i;
    // 使用循环将字符串中的每个字符转换为大写字母
    for (i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }
    // 打印大写化后的字符串
    printf("%s\n", str);

    return 0;
}

```

上面的代码输出为 `HELLO WORLD`，就是我们想要的结果。

## 深入探讨

在上面的例子中，我们通过使用循环来遍历字符串中的每个字符，然后使用 `toupper()`函数将它们转换为大写字母。但是，这种方法只适用于单词中没有空格的情况。如果字符串中有空格，我们需要另一种方法来处理。

一种解决方法是使用 `gets()`函数来接受用户输入的字符串，然后再使用循环和 `toupper()`函数来处理。但是，由于 `gets()`函数已被弃用，所以我们推荐使用 `fgets()`函数来接受输入。下面的代码展示了如何使用 `fgets()`函数来接受用户输入，并将输入的字符串转换为大写字母：

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // 声明一个字符串并预先分配内存
    char str[100];
    // 使用fgets()函数接受用户输入
    fgets(str, 100, stdin);
    // 使用循环和toupper()函数将字符串转换为大写字母
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }
    // 打印大写化后的字符串
    printf("%s\n", str);

    return 0;
}

```

上面的代码可以处理包含空格的字符串，并且使用 `fgets()`函数可以避免缓冲区溢出的问题。

## 参考链接

- [toupper()函数文档](https://www.cplusplus.com/reference/cctype/toupper/)
- [gets()函数文档](https://www.cplusplus.com/reference/cstdio/gets/)
- [fgets()函数文档](https://www.cplusplus.com/reference/cstdio/fgets/)