---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:17.560205-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 VBA \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528 Microsoft Scripting Runtime \u5E93\u4E2D\u63D0\u4F9B\u7684 `FileSystemObject`\
  \ \u6765\u5B9E\u73B0\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u3002\u8BE5\u5BF9\u8C61\
  \u63D0\u4F9B\u4E86\u521B\u5EFA\u3001\u8BFB\u53D6\u3001\u5199\u5165\u548C\u5220\u9664\
  \u6587\u4EF6\u548C\u6587\u4EF6\u5939\u7684\u65B9\u6CD5\u3002\u4EE5\u4E0B\u662F\u521B\
  \u5EFA\u4E34\u65F6\u6587\u4EF6\u7684\u5206\u6B65\u6307\u5357\uFF1A 1. **\u542F\u7528\
  \ Microsoft Scripting\u2026"
lastmod: '2024-03-13T22:44:47.598607-06:00'
model: gpt-4-0125-preview
summary: "\u5728 VBA \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528 Microsoft Scripting Runtime\
  \ \u5E93\u4E2D\u63D0\u4F9B\u7684 `FileSystemObject` \u6765\u5B9E\u73B0\u521B\u5EFA\
  \u4E34\u65F6\u6587\u4EF6\u3002\u8BE5\u5BF9\u8C61\u63D0\u4F9B\u4E86\u521B\u5EFA\u3001\
  \u8BFB\u53D6\u3001\u5199\u5165\u548C\u5220\u9664\u6587\u4EF6\u548C\u6587\u4EF6\u5939\
  \u7684\u65B9\u6CD5\u3002\u4EE5\u4E0B\u662F\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u7684\
  \u5206\u6B65\u6307\u5357\uFF1A\n\n1."
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## 如何操作：
在 VBA 中，可以使用 Microsoft Scripting Runtime 库中提供的 `FileSystemObject` 来实现创建临时文件。该对象提供了创建、读取、写入和删除文件和文件夹的方法。以下是创建临时文件的分步指南：

1. **启用 Microsoft Scripting Runtime**：首先，确保在您的 VBA 环境中启用了 Microsoft Scripting Runtime 参考。转到 VBA 编辑器中的工具 > 引用，并勾选 “Microsoft Scripting Runtime”。

2. **创建临时文件**：以下 VBA 代码演示如何在默认的临时文件夹中创建一个临时文件。

```vb
Sub CreateTemporaryFile()
    Dim fso As Object
    Dim tmpFile As Object
    
    ' 创建 FileSystemObject
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    ' 获取临时文件夹的路径
    Dim tempFolder As String
    tempFolder = fso.GetSpecialFolder(2) ' 2 表示临时文件夹
    
    ' 创建一个临时文件并获取对它的引用
    Set tmpFile = fso.CreateTextFile(tempFolder & "\myTempFile.txt", True)
    
    ' 向文件写入一些内容
    tmpFile.WriteLine "This is a test."
    
    ' 关闭文件
    tmpFile.Close
    
    ' 可选，打印路径以供参考
    Debug.Print "临时文件创建于：" & tempFolder & "\myTempFile.txt"
End Sub
```

3. **示例输出**：当你运行上述代码时，它会在临时文件夹中创建一个名为 `myTempFile.txt` 的临时文件，并向其写入一行文本。如果你打开了立即窗口（VBA 编辑器中的 `Ctrl + G`），你会看到：

```
临时文件创建于：C:\Users\[YourUsername]\AppData\Local\Temp\myTempFile.txt
```

## 深入探讨
所展示的方法使用了 Microsoft Scripting Runtime 的一部分 `FileSystemObject`（FSO）。FSO 是一个用于文件系统操作的强大工具，随 Visual Basic Scripting Edition 引入。尽管它已经有些年头了，但因其简单性和功能的广泛性，在 VBA 中仍然被广泛使用。

创建临时文件在许多编程和脚本任务中扮演着关键角色，为测试提供了一个沙箱环境或为不需要永久存储的过程提供了一个工作空间。然而，开发者应该小心处理这些文件，确保在不再需要时将它们移除或清除，以防止意外的数据泄露或不必要的磁盘空间消耗。

虽然 VBA 提供了处理文件和文件夹的原生方法，但 `FileSystemObject` 提供了一种更面向对象的方法，这对于来自其他语言的程序员可能更加熟悉。不过，对于处理临时文件的更加健壮或安全的方法，新技术或语言可能提供了更好的选择，如在 Python 或 .NET 等环境中使用内存数据结构或专用的临时文件库。在这些情况下，虽然 VBA 可以很好地用于快速任务或 Office 应用程序中的集成，但探索更广泛或安全敏感应用的替代方案是可取的。
