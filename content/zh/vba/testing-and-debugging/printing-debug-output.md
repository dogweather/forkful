---
title:                "打印调试输出"
aliases:
- /zh/vba/printing-debug-output.md
date:                  2024-02-01T21:58:47.894178-07:00
model:                 gpt-4-0125-preview
simple_title:         "打印调试输出"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何为何故？
在 Visual Basic for Applications（VBA）中打印调试输出涉及在代码中策略性地放置打印语句，以显示变量值、执行流程或自定义调试消息。这一技术对于调试至关重要，使程序员能够理解其代码在运行时的行为，并识别任何意外行为或错误。

## 如何操作：
在 VBA 中，`Debug.Print` 语句是向 Visual Basic 编辑器（VBE）的立即窗口打印调试信息的主力。为了有效使用此功能，你需要使立即窗口可见（视图 > 立即窗口或在 VBE 中按 `Ctrl+G`）。

这里有一个使用 `Debug.Print` 输出变量值和自定义消息的简单示例：

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "The value of sampleVar is: "; sampleVar
End Sub
```

当你运行这个子程序时，立即窗口将显示：
```
The value of sampleVar is: 42
```

你还可以使用它来跟踪复杂条件逻辑的流程，通过在代码的不同分支中插入 `Debug.Print` 语句：

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9

    If valueToCheck > 10 Then
        Debug.Print "Value is greater than 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "Value is between 1 and 9."
    Else
        Debug.Print "Value is 10 or less than 1."
    End If
End Sub
```

运行 `CheckValue` 会产生：
```
Value is between 1 and 9.
```

记住，`Debug.Print` 的输出只会到立即窗口，这在开发阶段非常有用，但不会出现在应用程序的任何面向用户的部分。

## 深入研究
立即窗口和 `Debug.Print` 方法在 Visual Basic for Applications 的历史中根深蒂固，反映了随时间演变的调试实践。最初，调试是一个更加文本化且视觉效果较少的过程，开发者严重依赖打印语句来理解他们的代码在做什么。多年来，随着开发环境的演进，调试工具也发展了，引入了断点、监视点和更复杂的性能分析工具，这些工具提供了对代码行为更直接、更交互式的见解。

尽管如此，`Debug.Print` 和立即窗口仍然非常有用，特别是在进行快速简单调试会话或处理难以断入的代码（如事件处理程序）时。话虽如此，重要的是要认识到，与利用带有断点、监视和堆栈检查功能的集成调试器相比，仅依靠打印语句进行调试在现代编程中可能效率较低。

虽然诸如日志框架或更高级的调试工具等替代方案提供了更多的功能和灵活性，但 VBA 中 `Debug.Print` 的简单性和即时性使其成为一种宝贵的工具，特别是对于已经习惯于基于打印的调试技术的其他语言程序员。然而，随着他们对 VBA 和 Visual Basic 编辑器越来越熟悉，探索可用的全部调试工具范围可以带来更有效和高效的问题解决。
