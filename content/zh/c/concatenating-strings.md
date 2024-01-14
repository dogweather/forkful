---
title:                "C: 串联字符串"
simple_title:         "串联字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么：字符串连接的重要性

Strings（字符串）是计算机编程中不可或缺的数据类型，它们表示文本或字符序列。在许多情况下，我们需要将多个字符串连接成一个更大的字符串。这可以通过一个称为“字符串连接”的操作来实现。字符串连接在许多实际的应用中都有很大的作用，因此学习如何实现它是很有必要的。

## 如何实现字符串连接

我们可以使用C语言中的一个重要函数`strcat()`来实现字符串连接。这个函数需要两个参数，第一个参数是要连接的目标字符串，第二个参数是要添加到目标字符串后面的字符串。下面是一个简单的例子：

```C
#include <stdio.h>
#include <string.h>

int main() {

    char str1[20] = "Hello"; 
    char str2[20] = "World";

    // 使用strcat()函数连接两个字符串
    strcat(str1, str2);

    // 打印结果
    printf("连接后的字符串是: %s", str1);

    return 0;
}
```

**输出：** 连接后的字符串是： HelloWorld

从上面的代码可以看出，我们首先声明两个字符串变量`str1`和`str2`，然后使用`strcat()`函数将`str2`连接到`str1`的末尾。最后，我们打印出连接后的结果。

## 深入了解字符串连接

除了`strcat()`函数外，C语言中还有一个`strncat()`函数可以实现字符串连接。它与`strcat()`函数的不同之处在于，我们可以指定要连接的字符串的长度。这有助于避免出现字符串溢出的情况，从而提高程序的安全性。下面是`strncat()`函数的语法：

`char *strncat(char *str1, const char *str2, size_t count)`

其中，`count`参数表示要连接的`str2`字符串的长度。我们还可以使用`strncpy()`函数来将字符串复制到目标字符串中，然后使用`strcat()`函数将两个字符串连接起来。

# 同类文章参考

1. [C语言教程 - 字符串连接](https://www.runoob.com/cprogramming/c-function-strcat.html)
2. [C语言教程 - 字符串函数](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)
3. [C语言字符串连接实现原理及注意事项](https://blog.csdn.net/u010103202/article/details/80330164)
4. [C语言函数： strcat()与strncat()函数的使用](https://www.cnblogs.com/mengyangyao/p/9240980.html)