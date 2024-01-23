---
title:                "读取命令行参数"
date:                  2024-01-20T17:55:32.487859-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在C语言中，读取命令行参数让你的程序能处理来自用户的输入。程序员们这么做是因为它能让程序更灵活，根据不同的参数执行不同的任务。

## How to: (怎么做：)
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("程序名：%s\n", argv[0]);
    printf("参数数量：%d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("参数 %d： %s\n", i, argv[i]);
    }
    return 0;
}
```
运行：`./your_program arg1 arg2 arg3`

输出：
```
程序名：./your_program
参数数量：3
参数 1： arg1
参数 2： arg2
参数 3： arg3
```

## Deep Dive (深入探讨)
命令行参数读取可以追溯到早期的Unix系统，是与操作系统交互的一种传统方式。除了`argc`和`argv`，你也可以用`getopt`函数来解析具有选项的参数。诸如`getopt_long`或第三方库能帮你处理更复杂的情况。实现时要注意，`argv`数组的最后一个元素后面是NULL指针，而且参数的内存是由系统管理的，不用你操心。

## See Also (参见)
- C Programming Language (2nd Edition) by Brian W. Kernighan and Dennis M. Ritchie
- GNU C Library: [Getopt](https://www.gnu.org/software/libc/manual/html_node/Getopt.html) 
- POSIX `getopt` function: [POSIX getopt](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html)
