---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:10.572326-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u4E5F\u88AB\u79F0\u4E3A\u8BFB\u53D6-\u6C42\
  \u503C-\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u5141\u8BB8\u7A0B\u5E8F\u5458\
  \u8F93\u5165\u8868\u8FBE\u5F0F\u6216\u4EE3\u7801\u5E76\u7ACB\u5373\u770B\u5230\u7ED3\
  \u679C\uFF0C\u589E\u5F3A\u4E86\u5B66\u4E60\u548C\u8C03\u8BD5\u8FC7\u7A0B\u3002\u5C3D\
  \u7BA1C\u8BED\u8A00\u4F20\u7EDF\u4E0A\u4E0D\u539F\u751F\u652F\u6301REPL\u73AF\u5883\
  \uFF0C\u73B0\u4EE3\u5DE5\u5177\u5F25\u8865\u4E86\u8FD9\u4E00\u5DEE\u8DDD\uFF0C\u63D0\
  \u4F9B\u4E86\u52A8\u6001\u63A2\u7D22C\u7A0B\u5E8F\u7684\u80FD\u529B\u3002"
lastmod: '2024-03-13T22:44:48.320456-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u4E5F\u88AB\u79F0\u4E3A\u8BFB\u53D6-\u6C42\
  \u503C-\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u5141\u8BB8\u7A0B\u5E8F\u5458\
  \u8F93\u5165\u8868\u8FBE\u5F0F\u6216\u4EE3\u7801\u5E76\u7ACB\u5373\u770B\u5230\u7ED3\
  \u679C\uFF0C\u589E\u5F3A\u4E86\u5B66\u4E60\u548C\u8C03\u8BD5\u8FC7\u7A0B\u3002\u5C3D\
  \u7BA1C\u8BED\u8A00\u4F20\u7EDF\u4E0A\u4E0D\u539F\u751F\u652F\u6301REPL\u73AF\u5883\
  \uFF0C\u73B0\u4EE3\u5DE5\u5177\u5F25\u8865\u4E86\u8FD9\u4E00\u5DEE\u8DDD\uFF0C\u63D0\
  \u4F9B\u4E86\u52A8\u6001\u63A2\u7D22C\u7A0B\u5E8F\u7684\u80FD\u529B\u3002"
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
