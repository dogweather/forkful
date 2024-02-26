---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:53.341864-07:00
description: "\u5728 PowerShell \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5305\u62EC\
  \u521B\u5EFA\u548C\u64CD\u4F5C\u57FA\u4E8E\u6587\u672C\u7684\u6587\u4EF6\uFF0C\u8FD9\
  \u662F\u65E5\u5FD7\u8BB0\u5F55\u3001\u6570\u636E\u5B58\u50A8\u548C\u914D\u7F6E\u811A\
  \u672C\u7F16\u5199\u7684\u57FA\u672C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u501F\u6B64\
  \u81EA\u52A8\u5316\u7CFB\u7EDF\u4EFB\u52A1\u3001\u6570\u636E\u5206\u6790\u4EE5\u53CA\
  \u4E0E\u5176\u4ED6\u5E94\u7528\u7A0B\u5E8F\u6216\u811A\u672C\u7684\u96C6\u6210\u3002"
lastmod: '2024-02-25T18:49:45.602298-07:00'
model: gpt-4-0125-preview
summary: "\u5728 PowerShell \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5305\u62EC\
  \u521B\u5EFA\u548C\u64CD\u4F5C\u57FA\u4E8E\u6587\u672C\u7684\u6587\u4EF6\uFF0C\u8FD9\
  \u662F\u65E5\u5FD7\u8BB0\u5F55\u3001\u6570\u636E\u5B58\u50A8\u548C\u914D\u7F6E\u811A\
  \u672C\u7F16\u5199\u7684\u57FA\u672C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u501F\u6B64\
  \u81EA\u52A8\u5316\u7CFB\u7EDF\u4EFB\u52A1\u3001\u6570\u636E\u5206\u6790\u4EE5\u53CA\
  \u4E0E\u5176\u4ED6\u5E94\u7528\u7A0B\u5E8F\u6216\u811A\u672C\u7684\u96C6\u6210\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么与为什么？
在 PowerShell 中写入文本文件包括创建和操作基于文本的文件，这是日志记录、数据存储和配置脚本编写的基本操作。程序员借此自动化系统任务、数据分析以及与其他应用程序或脚本的集成。

## 如何做：
PowerShell 为处理文件提供了直接的 cmdlet。主要使用 `Out-File` cmdlet 和重定向操作符来实现这一点。以下是在不同场景下向文件写入文本的示例：

**基本文本文件创建：**

要创建一个文本文件并向其写入一个简单的字符串，您可以使用：

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

或等效地使用重定向操作符：

```powershell
"Hello, World!" > .\example.txt
```

**向现有文件追加文本：**

如果你想在不覆盖的情况下将文本添加到现有文件的末尾：

```powershell
"另一行。" | Out-File -FilePath .\example.txt -Append
```

或使用追加重定向操作符：

```powershell
"另一行。" >> .\example.txt
```

**写入多行：**

要写入多行，可以使用字符串数组：

```powershell
$lines = "第一行", "第二行", "第三行"
$lines | Out-File -FilePath .\multilines.txt
```

**指定编码：**

要指定特定文本编码，请使用 `-Encoding` 参数：

```powershell
"使用 UTF8 编码的文本" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**使用第三方库：**

虽然 PowerShell 的内置 cmdlet 足以处理基本文件操作，但更复杂的任务可能会从第三方模块（如 `PowershellGet`）或为 Windows 移植的工具（如 `SED` 和 `AWK`）中受益。然而，对于纯粹写入文本文件而言，这些可能是过度的，通常不需要：

```powershell
# 假设一个更复杂的场景证明了使用外部库的必要性
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# 在这里进行更复杂的操作
```

_注意：总是考虑是否为你的需求添加第三方依赖的复杂性是合理的。_

**示例输出：**

执行完基本的文件创建命令后，检查 `example.txt` 的内容显示：

```plaintext
Hello, World!
```

追加文本后再检查 `example.txt`：

```plaintext
Hello, World!
另一行。
```
