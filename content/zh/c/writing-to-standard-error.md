---
title:                "标准错误写作"
html_title:           "C: 标准错误写作"
simple_title:         "标准错误写作"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是写到标准错误？

在C语言中，标准错误是指由程序直接生成的错误信息。与标准输出不同，标准错误允许程序在运行过程中打印出错误信息，以便我们更容易地发现并修复程序中的问题。因此，将错误信息写入标准错误是程序员经常使用的一种调试技术。

## 如何实现？

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "这是一个错误信息");
  return 0;
}
```

在上面的例子中，我们使用了fprintf()函数来将字符串"这是一个错误信息"写入标准错误。需要注意的是，我们使用的是stderr标准错误流，而不是stdout标准输出流。这样做可以确保程序在运行过程中直接打印错误信息，而不是将其与其他输出信息混合在一起。

执行上述代码，我们将会在控制台中看到如下输出：

```shell
这是一个错误信息
```

## 深入探讨

写入标准错误的技术可以追溯到早期的C语言。在早期的Unix操作系统中，程序员通常使用fprintf()函数来将错误信息写入标准错误，以便在命令行中方便地调试程序。随着时间的推移，更多的语言和系统都采用了类似的方法来处理错误信息。

除了使用fprintf()函数，程序员也可以使用其他特定于语言的方法来将错误信息写入标准错误。例如，Java中的System.err.println()函数可以直接将错误信息打印到控制台。

## 相关链接

如果你想了解更多关于标准错误的知识，请参考以下链接：

- [《C语言标准》](http://www.open-std.org/JTC1/sc22/wg14/www/docs/n1570.pdf)
- [《Unix编程环境》](https://book.douban.com/subject/25900403/)
- [《Java编程思想》](https://book.douban.com/subject/2130190/)