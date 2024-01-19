---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么?

命令行参数是在运行程序时输入的并被程序处理的数据。编程者之所以需要它，是因为这样可以使程序的运行根据用户的需要进行定制。

## 怎么做:

在 C 语言中, `main` 函数可以有两个参数来接收命令行值, 例如:

```C
int main(int argc, char *argv[]) { ... }
```

其中，`argc` 是输入参数的数量, 值至少为1, 因为第一个参数总是程序的名称。`argv` 是一个包含了所有命令行参数的字符串数组。

看看下面的例子:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("你输入了 %d 个参数：\n", argc);

    for(int i = 0; i < argc; i++) {
        printf("参数 #%d: %s\n", i, argv[i]);
    }

    return 0;
}
```

运行上述程序并输入参数 `/path/to/program argument1 argument2` 会得到下面的输出:

```
你输入了 3 个参数：
参数 #0: /path/to/program
参数 #1: argument1
参数 #2: argument2
```

## 深入探讨:

在历史上，C是一种系统编程语言，旧的 Unix 系统就是用 C 写的。因此，读取命令行参数是 C 语言设计的基础部分。

不过，处理命令行参数并非必须通过`main`函数的`argc`和`argv`参数。有些类库如`getopt`提供了更高级的解析功能。而使用Python或Bash script等更现代的脚本语言也可以轻松实现这一功能。

`argv`字符串数组的每个元素都是一个指针，指向字符串的开始。程序读取这些参数和字符串时并不会检查它们的有效性。因此，读取和处理这类参数时要当心溢出和其他类型的错误。

## 参考资料:

- C程序设计语言, Brian W. Kernighan and Dennis M. Ritchie (http://clc-wiki.net/wiki/K%26R2)
- GNU库C函数参考手册 (https://www.gnu.org/software/libc/manual/)
- C99标准 (ISO/IEC 9899:1999) (https://www.iso.org/standard/29237.html)