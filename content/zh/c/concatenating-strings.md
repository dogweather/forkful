---
title:                "C: 连接字符串"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

字符串连接是编程中非常常见的操作之一。它允许我们将多个字符串组合成一个更长的字符串，从而方便地处理文本数据。无论是在编写网页还是处理大量文本数据时，字符串连接都是必不可少的。

## 如何操作

C语言中的字符串连接可以通过使用`strcat()`函数来实现。这个函数需要两个参数，分别是要连接的字符串和要被连接的字符串。代码示例：

```C
char string1[20] = "Hello";
char string2[20] = " World!";
strcat(string1, string2);
printf("%s", string1);
```

输出将会是`Hello World!`。在这个示例中，我们将`string2`连接到了`string1`，并通过`printf`函数打印出来。

## 深入了解

在C语言中，字符串是一串连续的字符数组，最后以空字符`\0`结尾。因此，当我们使用`strcat()`函数时，它会首先寻找第一个字符串的`\0`，然后将第二个字符串从该位置开始复制到第一个字符串的末尾。这意味着第一个字符串的大小必须足够大，以容纳第二个字符串的字符。

另外，C语言中还有另一种字符串连接的方法，那就是使用`sprintf()`函数。这个函数可以将格式化的字符串输出到一个字符数组中，从而实现字符串连接的功能。代码示例：

```C
char string1[20] = "Hello";
char string2[20] = " World!";
sprintf(string1, "%s%s", string1, string2);
printf("%s", string1);
```

输出将会是`Hello World!`。在这个示例中，我们使用`sprintf()`函数将`string2`连接到了`string1`，并使用`printf()`函数打印出来。

## 参考文献

- 文章： [C语言字符串操作](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)
- 文章： [C语言格式化输出](https://www.runoob.com/cprogramming/c-function-sprintf.html)
- 文章： [C语言标准库函数strcat()](https://www.runoob.com/cprogramming/c-standard-library-string-strcat.html)

##参见

-  [C语言字符串操作函数](https://zh.wikipedia.org/wiki/%E5%AD%97%E7%AC%A6%E4%B8%B2%E6%93%8D%E4%BD%9C%E5%87%BD%E6%95%B0)
- [C语言格式化输出](https://zh.wikipedia.org/wiki/%E6%A0%BC%E5%BC%8F%E5%8C%96%E8%BE%93%E5%87%BA%E5%87%BD%E6%95%B0)