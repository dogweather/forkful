---
title:                "写入标准错误"
aliases:
- /zh/c/writing-to-standard-error.md
date:                  2024-02-03T18:15:04.751040-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么?

在C语言中写入标准错误是指将错误消息和诊断信息导向与主程序输出不同的流。程序员这样做是为了将错误消息与标准输出分开，使得两者更容易分别阅读和处理，尤其是在调试或记录程序执行时。

## 如何做：

在C语言中，使用`stderr`流来写入错误消息。不同于使用`printf`写入标准输出，写入`stderr`可以使用`fprintf`或`fputs`。以下是如何做到这一点的方法：

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "这是一条错误消息。\n");

    fputs("这是另一条错误消息。\n", stderr);
    
    return 0;
}
```

示例输出（到stderr）:
```
这是一条错误消息。
这是另一条错误消息。
```

需要注意的是，虽然输出在控制台与`stdout`看起来相似，但是当在终端使用重定向时，区别就变得明显了：

```sh
$ ./your_program > output.txt
```

这条命令只将标准输出重定向到`output.txt`，而错误消息仍然会显示在屏幕上。

## 深入了解

在基于Unix的系统中，`stdout`与`stderr`的区分可以追溯到C语言和Unix的早期。这种分离为更健壮的错误处理和日志记录提供了可能，因为它使程序员能够独立于标准程序输出重定向错误消息。尽管`stderr`默认为非缓冲，以确保错误消息立即输出，这在调试崩溃和其他关键问题时有帮助，但`stdout`通常是缓冲的，意味着其输出可能会延迟，直到缓冲区被刷新（例如，程序完成或手动刷新）。

在现代应用程序中，写入`stderr`仍然很重要，特别是对于命令行工具和服务器应用程序，在其中区分常规日志消息和错误至关重要。然而，对于更复杂的错误处理，特别是在GUI应用程序中或需要更复杂的日志记录机制的情况下，程序员可能会使用专门的日志记录库，这些库提供更多关于消息格式化、目的地（例如，文件、网络）和严重性级别（信息、警告、错误等）的控制。

虽然`stderr`为C语言中的错误报告提供了一个基本机制，但随着编程实践的进化和先进日志框架的可用性，它通常只是现代错误处理策略的起点。
