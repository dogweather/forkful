---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
当我们需要将多个字符串拼接在一起时，我们可以使用C语言中的字符串连接函数来拼接它们，这样可以简化我们的代码并且使代码更易读。

## 如何做
我们可以使用 C 语言中的 `strcat()` 函数来拼接字符串。下面是一个简单的例子：

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // 定义两个字符串
    char str1[] = "Hello";
    char str2[] = "World";

    // 使用 strcat() 函数拼接字符串
    strcat(str1, str2);

    printf("拼接后的结果是：%s", str1);

    return 0;
}

```

输出结果：
```
拼接后的结果是：HelloWorld
```

## 深入了解
当我们使用 `strcat()` 函数时，它会从第一个字符串的结尾开始将第二个字符串中的字符按顺序拼接到第一个字符串中，直到遇到第二个字符串的结尾符 `\0`。因此，我们需要确保第一个字符串的大小足够大，能够容纳下要拼接的两个字符串。

此外，如果第一个字符串的大小不够大，那么 `strcat()` 函数会出现缓冲区溢出的情况，导致程序崩溃或产生不可预知的结果。因此，在使用 `strcat()` 函数时，我们需要保证第一个字符串有足够的空间来容纳待拼接的字符串。

## 另请参阅
- [C语言字符串连接函数](https://www.runoob.com/cprogramming/c-function-strcat.html)
- [C语言字符串函数](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)