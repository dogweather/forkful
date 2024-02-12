---
title:                "读取命令行参数"
aliases: - /zh/vba/reading-command-line-arguments.md
date:                  2024-02-01T21:59:29.368794-07:00
model:                 gpt-4-0125-preview
simple_title:         "读取命令行参数"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
