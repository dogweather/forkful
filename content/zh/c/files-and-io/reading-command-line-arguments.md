---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:09.145575-07:00
description: "\u5728C\u7F16\u7A0B\u4E2D\uFF0C\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u5141\u8BB8\u7A0B\u5E8F\u76F4\u63A5\u4ECE\u7EC8\u7AEF\u63A5\u6536\u8F93\u5165\uFF0C\
  \u63D0\u9AD8\u4E86\u7075\u6D3B\u6027\u548C\u53EF\u7528\u6027\u3002\u7A0B\u5E8F\u5458\
  \u5229\u7528\u8FD9\u4E00\u70B9\u6765\u914D\u7F6E\u811A\u672C\u884C\u4E3A\uFF0C\u800C\
  \u65E0\u9700\u4FEE\u6539\u4EE3\u7801\uFF0C\u4F7F\u5F97\u5E94\u7528\u7A0B\u5E8F\u9002\
  \u5E94\u6027\u5F3A\u4E14\u9AD8\u6548\u3002"
lastmod: '2024-03-13T22:44:48.338383-06:00'
model: gpt-4-0125-preview
summary: "\u5728C\u7F16\u7A0B\u4E2D\uFF0C\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u5141\u8BB8\u7A0B\u5E8F\u76F4\u63A5\u4ECE\u7EC8\u7AEF\u63A5\u6536\u8F93\u5165\uFF0C\
  \u63D0\u9AD8\u4E86\u7075\u6D3B\u6027\u548C\u53EF\u7528\u6027\u3002\u7A0B\u5E8F\u5458\
  \u5229\u7528\u8FD9\u4E00\u70B9\u6765\u914D\u7F6E\u811A\u672C\u884C\u4E3A\uFF0C\u800C\
  \u65E0\u9700\u4FEE\u6539\u4EE3\u7801\uFF0C\u4F7F\u5F97\u5E94\u7528\u7A0B\u5E8F\u9002\
  \u5E94\u6027\u5F3A\u4E14\u9AD8\u6548\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
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
