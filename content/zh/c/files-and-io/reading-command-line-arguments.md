---
title:                "读取命令行参数"
aliases: - /zh/c/reading-command-line-arguments.md
date:                  2024-02-03T18:06:09.145575-07:00
model:                 gpt-4-0125-preview
simple_title:         "读取命令行参数"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在C编程中，读取命令行参数允许程序直接从终端接收输入，提高了灵活性和可用性。程序员利用这一点来配置脚本行为，而无需修改代码，使得应用程序适应性强且高效。

## 如何操作：

在C中，`main` 函数可以设计成使用参数 `int argc` 和 `char *argv[]` 来接受命令行参数。这里，`argc` 代表传递的参数数量，而 `argv` 是一个字符指针数组，列出了所有参数。下面是一个快速示例来说明：

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("程序名称: %s\n", argv[0]);
    printf("参数数量: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("参数 %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

使用上述代码，如果程序执行为 `./programName -a example`，输出将会是：

```
程序名称: ./programName
参数数量: 2
参数 1: -a
参数 2: example
```

这展示了如何在C程序中解析和利用命令行参数。

## 深入探究

将参数传递给程序的约定可以追溯到Unix的早期。在这种传统方法中，`argc` 和 `argv` 提供了一个简单却强大的命令行交互界面，体现了Unix关于小型、模块化实用程序共同工作的哲学。尽管现代语言经常引入更复杂的库或框架来解析命令行参数，但C的方法提供了无与伦比的透明度和控制力。

在近期的发展中，像POSIX系统中的 `getopt` 这样的库已经发展起来，以支持更复杂的解析需求，比如处理长选项名称或为缺失的参数提供默认值。然而，`argc` 和 `argv` 的基本机制对于理解C程序如何与其运行时环境交互仍然至关重要。

批评者可能会争论直接处理 `argc` 和 `argv` 可能容易出错，推动使用更高级的抽象。尽管如此，对于那些寻求掌握C的复杂性并欣赏其低级操作细微差别的人来说，掌握命令行参数解析是一种成人礼。这种历史方法论和实用性的结合，封装了C在系统编程和软件开发中持久吸引力的大部分。
