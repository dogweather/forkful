---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:47.894178-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 VBA \u4E2D\uFF0C`Debug.Print`\
  \ \u8BED\u53E5\u662F\u5411 Visual Basic \u7F16\u8F91\u5668\uFF08VBE\uFF09\u7684\u7ACB\
  \u5373\u7A97\u53E3\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u7684\u4E3B\u529B\u3002\u4E3A\
  \u4E86\u6709\u6548\u4F7F\u7528\u6B64\u529F\u80FD\uFF0C\u4F60\u9700\u8981\u4F7F\u7ACB\
  \u5373\u7A97\u53E3\u53EF\u89C1\uFF08\u89C6\u56FE > \u7ACB\u5373\u7A97\u53E3\u6216\
  \u5728 VBE \u4E2D\u6309 `Ctrl+G`\uFF09\u3002 \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\
  \u7528 `Debug.Print`\u2026"
lastmod: '2024-04-05T22:38:46.736690-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 VBA \u4E2D\uFF0C`Debug.Print` \u8BED\
  \u53E5\u662F\u5411 Visual Basic \u7F16\u8F91\u5668\uFF08VBE\uFF09\u7684\u7ACB\u5373\
  \u7A97\u53E3\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u7684\u4E3B\u529B\u3002\u4E3A\u4E86\
  \u6709\u6548\u4F7F\u7528\u6B64\u529F\u80FD\uFF0C\u4F60\u9700\u8981\u4F7F\u7ACB\u5373\
  \u7A97\u53E3\u53EF\u89C1\uFF08\u89C6\u56FE > \u7ACB\u5373\u7A97\u53E3\u6216\u5728\
  \ VBE \u4E2D\u6309 `Ctrl+G`\uFF09\u3002 \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528\
  \ `Debug.Print` \u8F93\u51FA\u53D8\u91CF\u503C\u548C\u81EA\u5B9A\u4E49\u6D88\u606F\
  \u7684\u7B80\u5355\u793A\u4F8B\uFF1A."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
