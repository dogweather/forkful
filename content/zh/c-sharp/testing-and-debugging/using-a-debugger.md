---
date: 2024-01-26 03:48:06.300830-07:00
description: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\u7528\u4E13\u95E8\
  \u5DE5\u5177\u6D4B\u8BD5\u548C\u8BCA\u65AD\u4EE3\u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u6D88\u9664\u9519\u8BEF\u3001\u7406\u89E3\u4EE3\u7801\
  \u6D41\u7A0B\uFF0C\u4EE5\u53CA\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u8868\u73B0\
  \u5982\u9884\u671F\u2014\u2014\u8FD9\u5C31\u50CF\u4E3A\u4F60\u7684\u4EE3\u7801\u5927\
  \u8111\u62E5\u6709\u4E00\u4E2A\u663E\u5FAE\u955C\u3002"
lastmod: '2024-02-25T18:49:45.340916-07:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\u7528\u4E13\u95E8\
  \u5DE5\u5177\u6D4B\u8BD5\u548C\u8BCA\u65AD\u4EE3\u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u6D88\u9664\u9519\u8BEF\u3001\u7406\u89E3\u4EE3\u7801\
  \u6D41\u7A0B\uFF0C\u4EE5\u53CA\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u8868\u73B0\
  \u5982\u9884\u671F\u2014\u2014\u8FD9\u5C31\u50CF\u4E3A\u4F60\u7684\u4EE3\u7801\u5927\
  \u8111\u62E5\u6709\u4E00\u4E2A\u663E\u5FAE\u955C\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
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
