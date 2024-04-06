---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:12.038557-07:00
description: "\u5982\u4F55\uFF1A \u8981\u5F00\u59CB\u5728 C \u4E2D\u8BFB\u53D6\u6587\
  \u672C\u6587\u4EF6\uFF0C\u4F60\u4E3B\u8981\u4F7F\u7528\u6807\u51C6 I/O \u5E93\u7684\
  \ `fopen()`\u3001`fgets()` \u548C `fclose()` \u51FD\u6570\u3002\u8FD9\u91CC\u6709\
  \u4E00\u4E2A\u7B80\u5355\u7684\u793A\u4F8B\uFF0C\u5B83\u8BFB\u53D6\u4E00\u4E2A\u540D\
  \u4E3A `example.txt` \u7684\u6587\u4EF6\u5E76\u5C06\u5176\u5185\u5BB9\u6253\u5370\
  \u5230\u6807\u51C6\u8F93\u51FA\uFF1A."
lastmod: '2024-04-05T21:53:48.618958-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
weight: 22
---

## 如何：
要开始在 C 中读取文本文件，你主要使用标准 I/O 库的 `fopen()`、`fgets()` 和 `fclose()` 函数。这里有一个简单的示例，它读取一个名为 `example.txt` 的文件并将其内容打印到标准输出：

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *filePointer;
    char buffer[255]; // 缓冲区，用于存储文本行

    // 以读模式打开文件
    filePointer = fopen("example.txt", "r");

    // 检查文件是否成功打开
    if (filePointer == NULL) {
        printf("无法打开文件。 \n");
        return 1;
    }

    while (fgets(buffer, 255, filePointer) != NULL) {
        printf("%s", buffer);
    }

    // 关闭文件以释放资源
    fclose(filePointer);
    return 0;
}
```

假设 `example.txt` 包含：
```
Hello, World!
Welcome to C programming.
```

输出结果将是：
```
Hello, World!
Welcome to C programming.
```

## 深入了解
C 中的文件读取有着悠久的历史，可以追溯到 Unix 的早期，当时文本流的简单性和优雅是基础。这导致文本文件被采用于多种用途，包括配置、日志记录和进程间通信。C 语言文件 I/O 库的简单性，通过如 `fopen()`、`fgets()` 和 `fclose()` 等函数的示例，凸显了其设计理念，即提供基本工具，让程序员能够用来构建复杂系统。

历史上，尽管这些功能在无数应用中发挥了很好的作用，现代编程实践已经凸显了一些限制，尤其是在错误处理、文件编码（例如，Unicode 支持）以及多线程应用中的并发访问方面。在其他语言中，或者甚至在 C 中使用像 `libuv` 或 `Boost.Asio` for C++ 这样的库所采用的替代方法，通过直接用更复杂的 I/O 管理能力解决这些问题，提供了更强大的解决方案，包括异步 I/O 操作，这可以大大提高涉及大量文件读取操作或 I/O 绑定任务的应用程序的性能。

尽管技术进步了，学习使用 C 中的标准 I/O 库读取文件仍然至关重要。它不仅有助于理解许多编程环境中适用的文件处理的基础，而且还提供了一个基础，使人们能够欣赏文件 I/O 操作的发展，并探索现代应用程序中文件处理的更复杂库和框架。
