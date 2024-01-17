---
title:                "创建临时文件"
html_title:           "C: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 创建临时文件

为了更有效地编写程序，程序员经常创建临时文件。临时文件是在计算机上临时存储数据的文件，它们被用来解决许多编程问题。在这篇文章中，我们将一起探讨如何创建临时文件以及为什么程序员会这样做。

## 什么是创建临时文件，为什么要这么做？

创建临时文件就是在计算机上临时存储数据的文件。程序员需要这么做的原因有很多，例如：

- 临时存储大量数据：当程序需要处理大量数据时，使用临时文件可以帮助节省内存空间。
- 临时存储数据结构：某些情况下，程序员可能需要在程序运行过程中创建临时数据结构来解决某个问题。
- 避免数据丢失：有些程序在运行过程中会产生临时数据，但这些数据并不需要长久保留，因此将它们存储到临时文件中可以避免数据丢失。

## 如何创建临时文件

要创建临时文件，我们可以使用C语言中的`tmpfile()`函数。这个函数将自动创建一个唯一的临时文件，并返回一个指向该文件的指针。

```C
#include <stdio.h>

int main() {
  FILE *fp; 
  char c;

  // 创建临时文件并向其中写入数据
  fp = tmpfile();
  fputc('H', fp); 
  fputc('i', fp);
  fputc('!', fp);
  
  // 读取临时文件中的数据，并输出
  rewind(fp);
  while ((c = fgetc(fp)) != EOF) {
    putchar(c);
  }
  
  // 关闭临时文件
  fclose(fp);
  return 0;
}
```

输出结果将会是`Hi!`。临时文件将在程序运行结束后自动删除。

## 深入探讨

- 历史上，程序员经常使用临时文件来解决内存不足的问题。随着计算机硬件技术的发展，这种情况变得越来越少，但仍然有些特殊情况下仍会使用临时文件。
- 除了使用`tmpfile()`函数，程序员还可以使用`tmpnam()`函数来创建临时文件。但由于这个函数并不是线程安全的，因此建议使用`tmpfile()`。
- 在某些操作系统中，临时文件会被存储在`/tmp`或者`/var/tmp`目录下，我们可以通过`tmpfile()`函数的返回值来获取临时文件的路径。

## 相关阅读

- [C语言中的文件操作](https://www.runoob.com/cprogramming/c-file-io.html)
- [tmpfile()函数的文档](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [tmpnam()函数的文档](https://www.cplusplus.com/reference/cstdio/tmpnam/)