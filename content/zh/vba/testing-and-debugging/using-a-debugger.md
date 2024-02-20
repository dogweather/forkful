---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:34.600469-07:00
description: "\u5728Visual Basic for Applications (VBA)\u4E2D\u4F7F\u7528\u8C03\u8BD5\
  \u5668\u5305\u62EC\u9010\u6B65\u6267\u884C\u4EE3\u7801\u4EE5\u68C0\u67E5\u5176\u6267\
  \u884C\u6D41\u7A0B\u548C\u53D8\u91CF\u72B6\u6001\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\
  \u4E8E\u8BC6\u522B\u548C\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u81F3\u5173\
  \u91CD\u8981\uFF0C\u6700\u7EC8\u786E\u4FDD\u5B83\u6309\u9884\u671F\u6267\u884C\u3002"
lastmod: 2024-02-19 22:05:06.600379
model: gpt-4-0125-preview
summary: "\u5728Visual Basic for Applications (VBA)\u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\
  \u5305\u62EC\u9010\u6B65\u6267\u884C\u4EE3\u7801\u4EE5\u68C0\u67E5\u5176\u6267\u884C\
  \u6D41\u7A0B\u548C\u53D8\u91CF\u72B6\u6001\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\
  \u8BC6\u522B\u548C\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u81F3\u5173\u91CD\
  \u8981\uFF0C\u6700\u7EC8\u786E\u4FDD\u5B83\u6309\u9884\u671F\u6267\u884C\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在Visual Basic for Applications (VBA)中使用调试器包括逐步执行代码以检查其执行流程和变量状态。这个过程对于识别和修复代码中的错误至关重要，最终确保它按预期执行。

## 如何操作:

在VBA中，调试器是Visual Basic编辑器(VBE)不可或缺的一部分。以下是您可以利用它的方法：

1. **设置断点**: 在您感兴趣的代码行旁边的左边距点击，或将光标放在该行上并按F9。这告诉VBA在达到这个点时暂停执行。

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' 在这里设置断点
        Next counter
    End Sub
    ```

    当代码执行时，它将在`Debug.Print counter`行暂停，允许您检查变量值。

2. **逐步调试 (F8)**: 使用此命令，您可以一次执行一条语句，进入任何被调用的程序。这对追踪您的代码和函数如何互动非常有用。

3. **监视窗口**: 使用监视窗口来监视变量或表达式的值。如果变量不在范围内，监视窗口将会指出。右击变量 > 添加监视。

4. **即时窗口 (Ctrl+G)**: 这个窗口特别适用于在调试时测试表达式或修改变量值。输入`?variableName`来打印一个变量的当前值，或用`variableName = newValue`赋予一个新值。

    ```vb
    ' 在即时窗口
    ?counter ' 打印counter的当前值
    counter = 3 ' 将counter的值设置为3
    ```

5. **示例输出**:

    当您到达断点并使用F8逐行执行时，即时窗口可能会显示类似如下的内容：

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    在这里，我们在每次迭代后手动查询了`counter`变量。

## 深入了解:

虽然VBA的调试器强大，但它是编程语言中调试工具较宽广传统的一部分，从最早的前身开始就有显著的发展。随着VBA最初版本的推出，其目标是为开发者提供一套简单却强大的代码检查和修正工具。随着时间的推移，增强功能包括条件断点、改进的监视功能，以及与Excel界面的集成，以更直观地检查数据。

然而，与Visual Studio或Eclipse这样的现代集成开发环境(IDEs)相比，VBA的调试工具可能看起来基础。这些现代IDE提供了更复杂的功能，如实时变量检查、高级断点和集成的单元测试框架。虽然这些替代品提供了更全面的调试体验，但VBA调试器的简单性和直接性仍然适合于Microsoft Office应用程序内的自动化和脚本编写的特定上下文。

对于习惯于这些现代环境的程序员来说，适应VBA的调试工具可能需要改变方法。然而，检查变量、逐步执行代码和观察运行时行为的基本原则是普遍适用的。通过实践，VBA的调试器成为确保您的自动化脚本在Office生态系统内无懈可击的不可或缺的工具。
