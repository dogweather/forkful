---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:28.132629-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u8BFB\u53D6\u6587\u672C\
  \u6587\u4EF6\u6D89\u53CA\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BBF\u95EE\u548C\u63D0\u53D6\
  \ Office \u5E94\u7528\u7A0B\u5E8F\u5185\u6587\u672C\u6587\u4EF6\u7684\u5185\u5BB9\
  \u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u5BFC\u5165\
  \u6216\u5904\u7406\u5B58\u50A8\u5728\u5E73\u9762\u6587\u4EF6\u4E2D\u7684\u6570\u636E\
  \uFF0C\u4FBF\u4E8E\u5728 Office \u751F\u6001\u7CFB\u7EDF\u5185\u76F4\u63A5\u8FDB\
  \u884C\u81EA\u52A8\u5316\u548C\u6570\u636E\u64CD\u4F5C\u3002"
lastmod: '2024-03-11T00:14:21.366669-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u8BFB\u53D6\u6587\u672C\
  \u6587\u4EF6\u6D89\u53CA\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u8BBF\u95EE\u548C\u63D0\u53D6\
  \ Office \u5E94\u7528\u7A0B\u5E8F\u5185\u6587\u672C\u6587\u4EF6\u7684\u5185\u5BB9\
  \u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u5BFC\u5165\
  \u6216\u5904\u7406\u5B58\u50A8\u5728\u5E73\u9762\u6587\u4EF6\u4E2D\u7684\u6570\u636E\
  \uFF0C\u4FBF\u4E8E\u5728 Office \u751F\u6001\u7CFB\u7EDF\u5185\u76F4\u63A5\u8FDB\
  \u884C\u81EA\u52A8\u5316\u548C\u6570\u636E\u64CD\u4F5C\u3002"
title: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Visual Basic for Applications (VBA) 中读取文本文件涉及以编程方式访问和提取 Office 应用程序内文本文件的内容。程序员经常执行此任务以导入或处理存储在平面文件中的数据，便于在 Office 生态系统内直接进行自动化和数据操作。

## 如何进行：

在 VBA 中读取文本文件最简单的方法是使用 `Open` 语句与 `Input` 或 `Line Input` 函数结合使用。以下是如何做到这一点：

1. **打开文件以供读取** - 首先，您需要打开文件。确保文件路径对应用程序可访问。

```basic
Open "C:\example.txt" For Input As #1
```

2. **读取文件内容** - 您可以使用 `Line Input` 逐行读取，也可以使用 `Input` 一次性读取整个文件。

- **逐行读取:**

```basic
Dim fileContent As String
While Not EOF(1) ' EOF = 文件结束
    Line Input #1, fileContent
    Debug.Print fileContent ' 将行输出到立即窗口
Wend
Close #1
```

- **一次性读取整个文件:**

```basic
Dim fileContent As String
Dim fileSize As Long
fileSize = LOF(1) ' LOF = 文件长度
If fileSize > 0 Then
    fileContent = Input(fileSize, #1)
    Debug.Print fileContent
End If
Close #1
```

3. **示例输出**:

假设 `example.txt` 包含：

```
Hello,
This is a sample text file.
Enjoy reading!
```

输出到立即窗口将根据您选择的方法是整个文本还是逐行。

## 深入探讨

VBA中读取文本文件已经是数十年来办公自动化任务的基石。虽然在 VBA 生态系统内展示的方法效率高，但与现代编程实践相比，可能显得古老，现代编程习惯常使用更高级的抽象或库来进行文件操作。例如，Python 在 `with` 语句内使用 `open()` 函数，提供了更清晰的语法和自动文件处理能力。

话虽如此，当在 Microsoft Office 环境的局限内工作时，VBA 提供了直接且原生的方法来操作文件，这对于需要与 Office 产品互操作的应用程序至关重要。无需外部库或复杂配置，就能简单地打开文本文件、逐行或全部读取并处理其内容的能力，让 VBA 成为 Office 开发人员工具箱中的宝贵工具。

虽然在现代编程语言中有更好的替代方案，可以更高效且用更少的代码处理文件，但理解和利用 VBA 读取文本文件的能力，可以显著提高生产力并扩展基于 Office 的应用程序的功能。
