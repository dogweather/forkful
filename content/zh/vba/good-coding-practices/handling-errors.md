---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:05.586122-07:00
description: "Visual Basic for Applications (VBA) \u4E2D\u7684\u9519\u8BEF\u5904\u7406\
  \u6307\u7684\u662F\u9884\u671F\u3001\u68C0\u6D4B\u548C\u89E3\u51B3\u7F16\u7A0B\u3001\
  \u5E94\u7528\u7A0B\u5E8F\u6216\u901A\u4FE1\u9519\u8BEF\u7684\u8FC7\u7A0B\u3002\u5B9E\
  \u73B0\u5F3A\u5927\u7684\u9519\u8BEF\u5904\u7406\u5BF9\u4E8E\u4FDD\u6301\u5E94\u7528\
  \u7A0B\u5E8F\u7684\u5B8C\u6574\u6027\u548C\u901A\u8FC7\u4F18\u96C5\u5730\u7BA1\u7406\
  \u610F\u5916\u95EE\u9898\u6539\u5584\u7528\u6237\u4F53\u9A8C\u81F3\u5173\u91CD\u8981\
  \uFF0C\u65E0\u9700\u5BFC\u81F4\u7A81\u7136\u5D29\u6E83\u6216\u6570\u636E\u4E22\u5931\
  \u3002"
lastmod: '2024-03-13T22:44:47.582688-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA) \u4E2D\u7684\u9519\u8BEF\u5904\u7406\
  \u6307\u7684\u662F\u9884\u671F\u3001\u68C0\u6D4B\u548C\u89E3\u51B3\u7F16\u7A0B\u3001\
  \u5E94\u7528\u7A0B\u5E8F\u6216\u901A\u4FE1\u9519\u8BEF\u7684\u8FC7\u7A0B\u3002\u5B9E\
  \u73B0\u5F3A\u5927\u7684\u9519\u8BEF\u5904\u7406\u5BF9\u4E8E\u4FDD\u6301\u5E94\u7528\
  \u7A0B\u5E8F\u7684\u5B8C\u6574\u6027\u548C\u901A\u8FC7\u4F18\u96C5\u5730\u7BA1\u7406\
  \u610F\u5916\u95EE\u9898\u6539\u5584\u7528\u6237\u4F53\u9A8C\u81F3\u5173\u91CD\u8981\
  \uFF0C\u65E0\u9700\u5BFC\u81F4\u7A81\u7136\u5D29\u6E83\u6216\u6570\u636E\u4E22\u5931\
  \u3002."
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
