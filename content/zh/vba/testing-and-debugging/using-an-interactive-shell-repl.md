---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:57.099001-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\
  \u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u5141\u8BB8\u7528\u6237\u8F93\u5165\u547D\u4EE4\
  \uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5B9E\u65F6\u67E5\u770B\u7ED3\u679C\u3002\
  \u7A0B\u5E8F\u5458\u5229\u7528REPL\u8FDB\u884C\u5FEB\u901F\u539F\u578B\u8BBE\u8BA1\
  \u3001\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u6216\u5728\u66F4\u4E92\u52A8\u548C\u8FED\
  \u4EE3\u7684\u73AF\u5883\u4E2D\u8C03\u8BD5\uFF0C\u4ECE\u800C\u63D0\u9AD8\u751F\u4EA7\
  \u529B\u548C\u5BF9\u4EE3\u7801\u7684\u7406\u89E3\u3002"
lastmod: '2024-03-13T22:44:47.575281-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\
  \u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u5141\u8BB8\u7528\u6237\u8F93\u5165\u547D\u4EE4\
  \uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5B9E\u65F6\u67E5\u770B\u7ED3\u679C\u3002\
  \u7A0B\u5E8F\u5458\u5229\u7528REPL\u8FDB\u884C\u5FEB\u901F\u539F\u578B\u8BBE\u8BA1\
  \u3001\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u6216\u5728\u66F4\u4E92\u52A8\u548C\u8FED\
  \u4EE3\u7684\u73AF\u5883\u4E2D\u8C03\u8BD5\uFF0C\u4ECE\u800C\u63D0\u9AD8\u751F\u4EA7\
  \u529B\u548C\u5BF9\u4EE3\u7801\u7684\u7406\u89E3\u3002"
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 什么和为什么？

交互式shell，或读取-求值-打印循环（REPL），允许用户输入命令，执行它们，并实时查看结果。程序员利用REPL进行快速原型设计、测试代码片段或在更互动和迭代的环境中调试，从而提高生产力和对代码的理解。

## 如何操作：

Visual Basic for Applications（VBA）本身并不原生支持像Python或JavaScript等语言所见的交互式shell或REPL体验。然而，您可以在一定程度上通过使用VBA IDE（集成开发环境）中的立即窗口来模拟这种体验。

**访问立即窗口：**
1. 在您的Office应用程序中按`Alt + F11`打开VBA IDE。
2. 如果立即窗口不可见，您可以通过按`Ctrl + G`或从视图菜单中选择它来打开。

**将立即窗口用作REPL：**
- 要执行一行代码，只需在立即窗口中输入它并按Enter键。例如：

```basic
Debug.Print 2 + 2
```

- 示例输出：
```
 4
```

- 您还可以调用在模块中定义的函数和子程序：

```basic
Public Sub SayHello()
    Debug.Print "Hello, World!"
End Sub
```

- 然后在立即窗口中：
```basic
Call SayHello
```

- 示例输出：
```
 Hello, World!
```

**注意：**立即窗口有限制。它非常适合快速测试和直接函数调用，但不支持直接在其中定义函数或子程序。复杂的调试和编程任务可能需要完整的模块开发。

## 深入探讨

尽管有其限制，但在 VBA 中的立即窗口作为与其他编程生态系统中的交互式 shells 最为接近的对应物。从历史上看，VBA 一直专注于通过脚本和宏扩展 Microsoft Office 应用程序的功能，而不是独立的软件开发，这可能解释了缺乏完整的 REPL。

对于需要广泛交互式测试或复杂逻辑开发的任务，其他配备了原生 REPL 支持的编程环境，如带有其 IDLE 的 Python，或带有 Node.js 的 JavaScript，可能提供更好的替代方案。这些环境不仅提供交互式 shell，还提供更强大的编程、调试和测试设施。

立即窗口确实为快速测试表达式、运行函数和直接操作 Office 应用程序对象提供了宝贵的工具。因此，它在 VBA 开发过程中占据了至关重要的位置，提供了传统编译-运行-调试周期所无法比拼的即时性和便利性，尽管其操作范围的限制是已知的。
