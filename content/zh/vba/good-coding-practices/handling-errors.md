---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:05.586122-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 VBA \u4E2D\uFF0C\u9519\u8BEF\u5904\
  \u7406\u901A\u5E38\u4F7F\u7528 `On Error` \u8BED\u53E5\u6765\u5B9E\u73B0\uFF0C\u8BE5\
  \u8BED\u53E5\u6307\u793A VBA \u5728\u53D1\u751F\u9519\u8BEF\u65F6\u5982\u4F55\u7EE7\
  \u7EED\u3002\u6700\u5E38\u89C1\u7684\u9519\u8BEF\u5904\u7406\u7B56\u7565\u5305\u62EC\
  \ `On Error GoTo` \u6807\u7B7E\u3001`On Error Resume Next` \u548C `On Error GoTo\
  \ 0`\u3002 **\u793A\u4F8B 1\uFF1A\u4F7F\u7528 `On\u2026"
lastmod: '2024-04-05T21:53:47.903462-06:00'
model: gpt-4-0125-preview
summary: "**\u793A\u4F8B 1\uFF1A\u4F7F\u7528 `On Error GoTo`** \u6B64\u65B9\u6CD5\u5141\
  \u8BB8\u60A8\u5728\u9047\u5230\u9519\u8BEF\u540E\u7ACB\u5373\u5C06\u7A0B\u5E8F\u6307\
  \u5411\u4EE3\u7801\u7684\u7279\u5B9A\u90E8\u5206\uFF0C\u8BE5\u90E8\u5206\u88AB\u6807\
  \u8BB0\u4E3A\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
在 VBA 中，错误处理通常使用 `On Error` 语句来实现，该语句指示 VBA 在发生错误时如何继续。最常见的错误处理策略包括 `On Error GoTo` 标签、`On Error Resume Next` 和 `On Error GoTo 0`。

**示例 1：使用 `On Error GoTo`**

此方法允许您在遇到错误后立即将程序指向代码的特定部分，该部分被标记为。

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' 这将导致除以零错误

    Exit Sub
ErrHandler:
    MsgBox "发生错误：" & Err.Description, vbCritical, "错误！"
    Resume Next
End Sub
```

在此示例中，任何运行时错误将触发跳转到 `ErrHandler`，显示错误消息，然后继续执行错误后的下一行。

**示例 2：使用 `On Error Resume Next`**

该策略指示 VBA 即使发生错误也继续执行下一行代码，这对于预期无害的错误或当您计划稍后在执行中处理错误时非常有用。

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' 这不会导致程序停止；错误被忽略
    
    ' 检查是否发生错误
    If Err.Number <> 0 Then
        MsgBox "发生错误：" & Err.Description, vbExclamation, "已处理错误"
        ' 重置错误
        Err.Clear
    End If
End Sub
```

在这种情况下，程序在出错时不会中断；它检查是否发生了错误，如果发生了则处理它，然后清除错误。

## 深入探讨
从历史上看，编程语言中的错误处理已从简单的 goto 语句发展为更复杂的机制，如 Java 和 C# 等语言中的异常。VBA 的错误处理虽然不像现代异常处理那样强大或灵活，但在该语言在 Microsoft Office 环境中自动化任务的上下文中发挥其作用。

VBA 错误处理的主要限制在于其有些繁琐和手动的方法，需要仔细放置错误处理代码和清晰理解执行流程。现代编程语言通常提供更优雅的解决方案，例如 try-catch 块，无需手动检查或代码执行中的跳转即可自动处理错误处理代码的流程。

尽管有这些限制，VBA 的错误处理机制适用于大多数自动化任务，当正确使用时，可以显著降低未处理错误对用户造成问题的可能性。此外，理解 VBA 的错误处理可以提供对旧编程范例和软件开发中错误处理策略进化的洞察。
