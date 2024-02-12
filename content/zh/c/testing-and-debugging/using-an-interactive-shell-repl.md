---
title:                "使用交互式Shell（REPL）"
aliases:
- /zh/c/using-an-interactive-shell-repl.md
date:                  2024-02-03T18:10:10.572326-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用交互式Shell（REPL）"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

交互式shell，也被称为读取-求值-打印循环（REPL），允许程序员输入表达式或代码并立即看到结果，增强了学习和调试过程。尽管C语言传统上不原生支持REPL环境，现代工具弥补了这一差距，提供了动态探索C程序的能力。

## 如何操作：

想要使用C语言的REPL，可能不像在Python或JavaScript这样的语言中那样直接。然而，像`Cling`这样的工具，一个基于Clang和LLVM技术的C/C++解释器，使其成为可能。以下是如何开始：

1. **安装Cling**：根据您的操作系统，您可能会在包管理器中找到Cling，或需要从源码构建。例如，在Ubuntu上，可以简单地执行`sudo apt-get install cling`。

2. **启动Cling**：打开您的终端并输入`cling`以启动交互式shell。

```bash
$ cling
```

3. **编写代码**：现在您可以直接在shell中输入C代码并立即看到结果。这里有一个简单的例子：

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Hello, REPL world!\n");
Hello, REPL world!
```

4. **使用变量和操作的示例**：实验变量并看到即时反馈。

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **包含库**：Cling允许您即时包含库，从而启用广泛的C功能。

```c
[cling]$ #include <math.h>
[cling]$ printf("Square root of %f is %f\n", 4.0, sqrt(4.0));
Square root of 4.000000 is 2.000000
```

## 深入探讨：

REPL环境的创始可以追溯到1960年代的Lisp，旨在支持交互式代码评估。然而，C语言的静态和编译性质对实现类似的代码执行即时调整提出了挑战。Cling及其他C/C++解释器的开发标志着向静态类型语言集成动态评估方面的重大进展。

值得注意的是，使用像Cling这样的解释器可能不会完全反映编译后C代码的行为，因为优化和执行上的差异。此外，虽然对于教学目的、快速原型设计和调试非常有价值，但与传统的编译-运行-调试周期相比，C语言的REPL在生产级代码开发中有时可能更慢且不太实用。

交互式C编程的替代方法包括编写小型、自包含的程序和使用集成调试工具的强大IDE，这可以提供更多控制和对执行的洞察，尽管缺乏即时性。尽管有这些替代方案，C语言REPL环境的出现代表了该语言多功能性的激动人心的扩展，迎合了现代对开发周期的灵活性和速度的需求。
