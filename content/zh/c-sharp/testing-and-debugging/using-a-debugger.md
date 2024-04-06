---
date: 2024-01-26 03:48:06.300830-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8C03\u8BD5\u7684\u5386\u53F2\u53EF\u4EE5\
  \u8FFD\u6EAF\u5230 1940 \u5E74\u4EE3\uFF0C\u5F53\u65F6\u4E00\u4E2A\u771F\u6B63\u7684\
  \u9519\u8BEF\uFF08\u4E00\u53EA\u98DE\u86FE\uFF09\u88AB\u53D1\u73B0\u5728\u4E00\u53F0\
  \u65E9\u671F\u7684\u8BA1\u7B97\u673A\u4E2D\u3002\u4ECA\u5929\u7684\u8C03\u8BD5\u5668\
  \uFF0C\u50CF Visual Studio \u4E2D\u7684\u8C03\u8BD5\u5668\uFF0C\u63D0\u4F9B\u4E86\
  \u4E00\u5957\u5F3A\u5927\u7684\u529F\u80FD\uFF0C\u5305\u62EC\u65AD\u70B9\u3001\u9010\
  \u6B65\u6267\u884C\u3001\u89C2\u5BDF\u7A97\u53E3\u7B49\u7B49\u3002 Visual Studio\
  \ \u8C03\u8BD5\u5668\u7684\u66FF\u4EE3\u54C1\u5305\u62EC\u7528\u4E8E C \u98CE\u683C\
  \u8BED\u8A00\u7684\u5F00\u6E90\u9009\u9879\u5982\u2026"
lastmod: '2024-04-05T22:51:00.981327-06:00'
model: gpt-4-0125-preview
summary: "Visual Studio \u8C03\u8BD5\u5668\u7684\u66FF\u4EE3\u54C1\u5305\u62EC\u7528\
  \u4E8E C \u98CE\u683C\u8BED\u8A00\u7684\u5F00\u6E90\u9009\u9879\u5982 GDB\uFF0C\u6216\
  \u7528\u4E8E Python \u7684 pdb\uFF0C\u4EE5\u53CA\u8DE8\u5E73\u53F0 IDE \u5982 JetBrains\
  \ Rider \u6216 VS Code\uFF0C\u8FD9\u4E9B\u90FD\u4E3A C# \u548C\u5176\u4ED6\u8BED\
  \u8A00\u63D0\u4F9B\u8C03\u8BD5\u5DE5\u5177\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
想象你有一个小程序行为不正常：

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // 哎呀，应该是 a + b
}
```

使用 Visual Studio 的调试器，在 `return a + a;` 旁边的左边距单击来设置一个断点。当你以 F5 运行程序时，执行将在那里暂停。将鼠标悬停在变量上以查看它们的值，或使用立即窗口评估表达式。你会看到 `a` 是 1，`b` 是 2，但 `a + a` 并不是我们预期的总和。将其改为 `a + b`，继续运行（F5），瞧，控制台输出 3。

## 深入探索
调试的历史可以追溯到 1940 年代，当时一个真正的错误（一只飞蛾）被发现在一台早期的计算机中。今天的调试器，像 Visual Studio 中的调试器，提供了一套强大的功能，包括断点、逐步执行、观察窗口等等。

Visual Studio 调试器的替代品包括用于 C 风格语言的开源选项如 GDB，或用于 Python 的 pdb，以及跨平台 IDE 如 JetBrains Rider 或 VS Code，这些都为 C# 和其他语言提供调试工具。

当你深入研究调试器的实现时，你看到的是一个附加到你的应用程序进程的程序。它解释机器代码、管理内存状态并控制执行流。这是重要的材料，对于有效的调试至关重要，这就是为什么调试模式通常运行得比释放模式慢，因为在释放模式中这些钩子并不存在。

## 另见
- [Visual Studio 调试器文档](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [调试策略](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
