---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:10.572326-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u60F3\u8981\u4F7F\u7528C\u8BED\u8A00\
  \u7684REPL\uFF0C\u53EF\u80FD\u4E0D\u50CF\u5728Python\u6216JavaScript\u8FD9\u6837\
  \u7684\u8BED\u8A00\u4E2D\u90A3\u6837\u76F4\u63A5\u3002\u7136\u800C\uFF0C\u50CF`Cling`\u8FD9\
  \u6837\u7684\u5DE5\u5177\uFF0C\u4E00\u4E2A\u57FA\u4E8EClang\u548CLLVM\u6280\u672F\
  \u7684C/C++\u89E3\u91CA\u5668\uFF0C\u4F7F\u5176\u6210\u4E3A\u53EF\u80FD\u3002\u4EE5\
  \u4E0B\u662F\u5982\u4F55\u5F00\u59CB\uFF1A 1.\u2026"
lastmod: '2024-04-05T21:53:48.597272-06:00'
model: gpt-4-0125-preview
summary: "**\u5B89\u88C5Cling**\uFF1A\u6839\u636E\u60A8\u7684\u64CD\u4F5C\u7CFB\u7EDF\
  \uFF0C\u60A8\u53EF\u80FD\u4F1A\u5728\u5305\u7BA1\u7406\u5668\u4E2D\u627E\u5230Cling\uFF0C\
  \u6216\u9700\u8981\u4ECE\u6E90\u7801\u6784\u5EFA\u3002\u4F8B\u5982\uFF0C\u5728Ubuntu\u4E0A\
  \uFF0C\u53EF\u4EE5\u7B80\u5355\u5730\u6267\u884C`sudo apt-get install cling`."
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
