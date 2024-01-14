---
title:    "C: 连接字符串"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么要用C语言连接字符串？
连接字符串是一种常见的技术，它可以将多个字符串拼接成一个更长的字符串。这在处理文本数据时特别有用，可以帮助我们更有效地处理文本文件或用户输入。在C语言中，我们可以使用标准库函数strcat()实现字符串连接操作。

## 如何连接字符串？
在C语言中，字符串是以字符数组的形式存储的，它们以空字符（'\0'）作为结尾。要连接两个字符串，我们可以使用strcat()函数，它需要两个参数：目标字符串和源字符串。下面是一个连接两个字符串的例子：

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str1[20] = "Hello";
  char str2[20] = "World";

  strcat(str1, str2);

  printf("Concatenated string: %s", str1);

  return 0;
}
```

输出结果应该是：

```C
Concatenated string: HelloWorld
```

## 深入了解字符串连接
当我们调用strcat()函数时，它会从目标字符串的空字符处开始将源字符串的第一个字符复制过来，一直复制到源字符串的结尾，然后加上一个空字符。这样，就实现了两个字符串的连接。

需要注意的是，源字符串的长度不能超过目标字符串的剩余空间，否则程序会发生未定义的行为。也需要确保目标字符串有足够的空间来存储连接后的字符串。

# 参考链接
- C语言标准库手册：https://www.cplusplus.com/reference/cstring/strcat/
- 字符串连接教程：https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm

# 参见
- 字符串拷贝：https://github.com/Languages4me/tutorials/tree/master/C/Strings/Copying
- 字符串比较：https://github.com/Languages4me/tutorials/tree/master/C/Strings/Comparing