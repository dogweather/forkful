---
title:                "将字符串转换为大写"
html_title:           "C: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当我们处理字符串时，有时候需要将其中的某些字符大写，这样可以让字符串更易读并且与其他字符串进行比较。这篇文章将会介绍如何使用C语言来实现字符串的大写操作。

## 如何实现字符串大写

首先，我们需要声明一个字符串变量，这样我们就可以在其上进行操作。然后，我们可以使用C语言内置的函数`toupper()`来将字符串中的字符转换为大写形式。下面是一个简单的例子：

```C
#include <stdio.h>
#include <ctype.h>
 
int main()
{
    char str[20] = "hello world";
    int i = 0;

    // 使用循环将字符串中的每个字符都转换为大写
    while (str[i])
    {
        str[i] = toupper(str[i]);
        i++;
    }

    // 输出结果 "HELLO WORLD"
    printf("%s\n", str);

    return 0;
}
```

这个例子中，我们使用了`while`循环来遍历字符串中的每个字符，并在循环中使用了`toupper()`函数来将字符转换为大写形式。在循环结束后，我们再次输出字符串，这时就会显示出转换后的大写形式。

## 更深入地了解字符串大写

在C语言中，字符串实际上是由字符数组来实现的。这意味着我们可以像处理数组一样来处理字符串，例如通过索引来访问特定字符。因此，我们也可以使用循环以外的方法来进行字符串的大写操作，例如使用`strcpy()`函数来复制字符串到一个新的变量中，并在其中进行大写转换。此外，`toupper()`函数只能将字符转换为大写形式，如果我们需要将字符串中的某些字符转换为小写形式，可以使用`tolower()`函数来实现。

## 查看更多

- [C语言字符串大写操作教程](https://www.runoob.com/cprogramming/c-function-toupper.html)
- [C语言字符串教程](https://www.runoob.com/cprogramming/c-strings.html)