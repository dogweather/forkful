---
title:                "使用调试器"
aliases:
- zh/c-sharp/using-a-debugger.md
date:                  2024-01-26T03:48:06.300830-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用调试器意味着利用专门工具测试和诊断代码。程序员这样做是为了消除错误、理解代码流程，以及确保他们的代码表现如预期——这就像为你的代码大脑拥有一个显微镜。

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
