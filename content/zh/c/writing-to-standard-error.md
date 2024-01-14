---
title:                "C: 向标准错误写入"
simple_title:         "向标准错误写入"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么会写到标准错误

在编程中，我们经常需要输出信息到控制台来帮助我们了解程序的运行情况。但是有时候，我们需要更详细的信息，比如错误信息，这时候就需要将信息输出到标准错误。这能帮助我们更快地定位和解决程序中的错误。

## 如何写到标准错误

要将信息写到标准错误，我们需要使用标准库函数`fprintf()`，它的参数包括文件指针和要输出的信息。下面是一个简单的例子：

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "这是一条错误信息");
    return 0;
}
```

运行上面的代码，我们就可以在控制台看到输出的错误信息`这是一条错误信息`。

## 深入了解写到标准错误

标准错误实际上是一个文件指针，即`stderr`。在大多数操作系统中，它默认指向控制台。除了使用`fprintf()`，我们还可以使用`fprintf()`函数的变体`fputs()`来输出字符串到标准错误。在调试程序时，我们可以使用`perror()`函数来输出系统错误信息，它会自动将错误信息和对应的错误码输出到标准错误。

## 另请参阅

- [fprintf() 官方文档](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [perror() 官方文档](https://www.cplusplus.com/reference/cstdio/perror/)