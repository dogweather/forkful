---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:29.368794-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u8BFB\u53D6\u547D\u4EE4\
  \u884C\u53C2\u6570\u5305\u62EC\u8BBF\u95EE\u5728\u6267\u884C\u7A0B\u5E8F\u65F6\u4F20\
  \u9012\u7ED9\u7A0B\u5E8F\u7684\u53C2\u6570\u3002\u8FD9\u79CD\u6280\u672F\u5E38\u7528\
  \u4E8E\u5728\u65E0\u9700\u7528\u6237\u4EA4\u4E92\u7684\u60C5\u51B5\u4E0B\u5F71\u54CD\
  \u7A0B\u5E8F\u7684\u884C\u4E3A\u6216\u8F93\u51FA\uFF0C\u4F7F\u81EA\u52A8\u5316\u548C\
  \u811A\u672C\u4EFB\u52A1\u53D8\u5F97\u66F4\u52A0\u7B80\u5355\u548C\u591A\u7528\u9014\
  \u3002"
lastmod: '2024-03-13T22:44:47.593010-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u8BFB\u53D6\u547D\u4EE4\
  \u884C\u53C2\u6570\u5305\u62EC\u8BBF\u95EE\u5728\u6267\u884C\u7A0B\u5E8F\u65F6\u4F20\
  \u9012\u7ED9\u7A0B\u5E8F\u7684\u53C2\u6570\u3002\u8FD9\u79CD\u6280\u672F\u5E38\u7528\
  \u4E8E\u5728\u65E0\u9700\u7528\u6237\u4EA4\u4E92\u7684\u60C5\u51B5\u4E0B\u5F71\u54CD\
  \u7A0B\u5E8F\u7684\u884C\u4E3A\u6216\u8F93\u51FA\uFF0C\u4F7F\u81EA\u52A8\u5316\u548C\
  \u811A\u672C\u4EFB\u52A1\u53D8\u5F97\u66F4\u52A0\u7B80\u5355\u548C\u591A\u7528\u9014\
  \u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 Visual Basic for Applications (VBA) 中读取命令行参数包括访问在执行程序时传递给程序的参数。这种技术常用于在无需用户交互的情况下影响程序的行为或输出，使自动化和脚本任务变得更加简单和多用途。

## 如何操作:

与更直接的编程环境不同，VBA 没有内置的功能可以直接以常规意义上读取命令行参数，因为它主要是为嵌入到 Microsoft Office 应用程序而设计的。然而，通过一点创造性，我们可以使用 Windows Script Host (WSH) 或调用外部 API 来实现类似的功能。这里有一个使用 WSH 的实用变通方法：

1. **创建 VBScript 以将参数传递给 VBA：**

   首先，写一个 VBScript 文件 (*yourScript.vbs*) 用来启动你的 VBA 应用程序（例如，一个 Excel 宏）并传递命令行参数：

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **在 VBA 中访问参数：**

   在你的 VBA 应用程序 (*YourMacroWorkbook.xlsm*) 中，修改或创建宏 (*YourMacroName*) 以接受参数：

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "参数 1： " & arg1 & " 参数 2： " & arg2
End Sub
```

3. **运行你的脚本：**

   从命令行执行 VBScript，根据需要传递参数：

```shell
cscript yourScript.vbs "Hello" "World"
```

   这应该会导致你的 VBA 宏执行，带有参数 "Hello" 和 "World"，并在消息框中显示它们。

## 深入了解：

在历史背景下，VBA 被设计用来扩展 Microsoft Office 应用程序的功能，并不是作为一个独立的编程环境。因此，直接与命令行交互超出了其主要范围，这解释了为什么缺乏内置支持读取命令行参数。

上面概述的方法，虽然有效，但更多的是一种变通方法，而不是原生解决方案，利用外部脚本来弥合差距。这种方法可能引入复杂性和潜在的安全问题，因为它需要启用宏，并可能需要降低安全设置才能执行。

对于高度依赖命令行参数或需要与 Windows 操作系统更无缝集成的任务，其他编程语言如 PowerShell 或 Python 可能提供更加健壮和安全的解决方案。这些替代品直接支持命令行参数，并且更适合需要外部输入动态修改其行为的独立应用程序或脚本。
