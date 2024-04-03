---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:13.439524-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A VBA \u63D0\u4F9B\u4E86\u51E0\u79CD\u5199\
  \u5165\u6587\u4EF6\u7684\u65B9\u6CD5\uFF0C\u4F46\u4F7F\u7528 `FileSystemObject`\
  \ \u662F\u5176\u4E2D\u4E00\u79CD\u6700\u76F4\u63A5\u7684\u65B9\u5F0F\u3002\u4EE5\
  \u4E0B\u662F\u4E00\u4E2A\u521B\u5EFA\u7B80\u5355\u6587\u672C\u6587\u4EF6\u5E76\u5411\
  \u5176\u5199\u5165\u6570\u636E\u7684\u5206\u6B65\u6307\u5357\uFF1A 1. **\u5F15\u7528\
  \ Microsoft \u811A\u672C\u8FD0\u884C\u65F6**\uFF1A\u9996\u5148\uFF0C\u786E\u4FDD\
  \u4F60\u7684 VBA \u7F16\u8F91\u5668\u53EF\u4EE5\u8BBF\u95EE `FileSystemObject`\u3002\
  \u5728\u2026"
lastmod: '2024-03-13T22:44:47.597229-06:00'
model: gpt-4-0125-preview
summary: "VBA \u63D0\u4F9B\u4E86\u51E0\u79CD\u5199\u5165\u6587\u4EF6\u7684\u65B9\u6CD5\
  \uFF0C\u4F46\u4F7F\u7528 `FileSystemObject` \u662F\u5176\u4E2D\u4E00\u79CD\u6700\
  \u76F4\u63A5\u7684\u65B9\u5F0F\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u521B\u5EFA\u7B80\
  \u5355\u6587\u672C\u6587\u4EF6\u5E76\u5411\u5176\u5199\u5165\u6570\u636E\u7684\u5206\
  \u6B65\u6307\u5357\uFF1A\n\n1."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何操作：
VBA 提供了几种写入文件的方法，但使用 `FileSystemObject` 是其中一种最直接的方式。以下是一个创建简单文本文件并向其写入数据的分步指南：

1. **引用 Microsoft 脚本运行时**：首先，确保你的 VBA 编辑器可以访问 `FileSystemObject`。在 VBA 编辑器中，转到 工具 > 引用 并勾选 "Microsoft Scripting Runtime"。

2. **创建文本文件**：以下 VBA 代码片段展示了如何创建一个文本文件并向其中写入一行文本。

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' CreateTextFile 参数：(文件名, 覆盖, Unicode)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' 写入一行文本
    textFile.WriteLine "Hello, VBA!"
    
    ' 关闭文件
    textFile.Close
End Sub
```

此脚本创建（或覆盖已存在的文件）一个名为 `example.txt` 的文件，在指定目录中写入 "Hello, VBA!"，然后关闭文件以保存更改。

3. **示例输出**：

运行上述 VBA 脚本后，你会发现一个名为 `example.txt` 的文件，内容如下：

```
Hello, VBA!
```

## 深入了解：
`FileSystemObject`（FSO），作为 Microsoft Scripting Runtime 库的一部分，为文件操作提供了丰富的属性和方法集，超出了传统 VBA 文件处理（例如，`Open`、`Print` #、`Write` #）所提供的。除了处理文件，FSO 还可以操作文件夹和驱动器，使其成为 VBA 内文件系统操作的强大工具。

但值得注意的是，虽然 FSO 在 VBA 中为文件操作提供了一种更现代的方法，但与 VBA 的原生文件处理语句相比，它可能为简单任务引入了额外的开销。此外，由于 FSO 是外部库的一部分，可移植性和与其他系统的兼容性（例如，早期版本的 Office，Mac Office）可能是关切点。

在性能、兼容性或最小外部依赖性至关重要的情境下，程序员可能考虑使用 VBA 的内置文件处理技术。然而，在这些担忧被减轻的环境中（如受控的企业设置），FileSystemObject 的好处往往超过其缺点。
