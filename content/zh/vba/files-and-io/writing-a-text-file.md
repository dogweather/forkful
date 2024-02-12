---
title:                "编写文本文件"
aliases: - /zh/vba/writing-a-text-file.md
date:                  2024-02-01T22:08:13.439524-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写文本文件"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/writing-a-text-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

在 Visual Basic for Applications (VBA) 中编写文本文件涉及创建、修改或向文件追加文本数据，这是存储输出、记录或与其他应用程序交互的基本任务。程序员利用这一功能来自动化报告、数据导出或在 Microsoft Office 生态系统内生成配置文件。

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
