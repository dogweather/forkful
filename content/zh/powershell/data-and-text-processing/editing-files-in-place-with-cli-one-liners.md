---
date: 2024-01-27 16:20:39.385824-07:00
description: "\u5728 PowerShell \u4E2D\u4F7F\u7528 CLI \u5355\u884C\u547D\u4EE4\u5C31\
  \u5730\u7F16\u8F91\u6587\u4EF6\uFF0C\u662F\u6307\u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\
  \u5BF9\u6587\u4EF6\u8FDB\u884C\u4FEE\u6539\uFF0C\u65E0\u9700\u5728\u7F16\u8F91\u5668\
  \u4E2D\u6253\u5F00\u5B83\u4EEC\u3002\u8FD9\u79CD\u65B9\u6CD5\u8282\u7701\u65F6\u95F4\
  \uFF0C\u5E76\u4E14\u5BF9\u4E8E\u6279\u5904\u7406\u6216\u81EA\u52A8\u5316\u591A\u4E2A\
  \u6587\u4EF6\u4E2D\u7684\u91CD\u590D\u7F16\u8F91\u4EFB\u52A1\u7279\u522B\u6709\u7528\
  \u3002"
lastmod: '2024-03-13T22:44:48.008755-06:00'
model: gpt-4-0125-preview
summary: "\u5728 PowerShell \u4E2D\u4F7F\u7528 CLI \u5355\u884C\u547D\u4EE4\u5C31\u5730\
  \u7F16\u8F91\u6587\u4EF6\uFF0C\u662F\u6307\u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\u5BF9\
  \u6587\u4EF6\u8FDB\u884C\u4FEE\u6539\uFF0C\u65E0\u9700\u5728\u7F16\u8F91\u5668\u4E2D\
  \u6253\u5F00\u5B83\u4EEC\u3002\u8FD9\u79CD\u65B9\u6CD5\u8282\u7701\u65F6\u95F4\uFF0C\
  \u5E76\u4E14\u5BF9\u4E8E\u6279\u5904\u7406\u6216\u81EA\u52A8\u5316\u591A\u4E2A\u6587\
  \u4EF6\u4E2D\u7684\u91CD\u590D\u7F16\u8F91\u4EFB\u52A1\u7279\u522B\u6709\u7528\u3002"
title: "\u4F7F\u7528\u547D\u4EE4\u884C\u4E00\u884C\u547D\u4EE4\u5C31\u5730\u7F16\u8F91\
  \u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 PowerShell 中使用 CLI 单行命令就地编辑文件，是指直接从命令行对文件进行修改，无需在编辑器中打开它们。这种方法节省时间，并且对于批处理或自动化多个文件中的重复编辑任务特别有用。

## 如何操作：

### 在单个文件中替换文本

让我们从一个简单的任务开始：您想要在名为 example.txt 的文件中替换所有的 "oldtext" 为 "newtext"。以下是如何做到这一点：

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

这个单行命令读取内容，执行替换操作，然后将内容写回原文件。

### 编辑多个文件

如果您需要对多个文件应用相同的更改怎么办？这里有一个使用循环的方法：

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

这段代码找到当前目录中的所有 `.txt` 文件，将每个文件中的 "oldtext" 替换为 "newtext"。

### 在文件的开头或结尾添加内容

添加或前置内容也可以简化处理：

```PowerShell
# 前置
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# 添加
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

在这里，我们简单地在现有内容之前或之后连接新内容，并保存回去。

## 深入探讨

历史上，就地编辑更常与 Unix 工具如 `sed` 和 `awk` 关联。作为较新的参与者，PowerShell 没有默认包含专门的就地编辑功能。这部分是因为其设计哲学，强调对象重于文本流，不像 Unix 工具那样将大多数输入视为文本。

对于这项任务，PowerShell 的替代品包括通过 Cygwin 或 Windows 子系统 Linux (WSL) 在 Windows 上可用的传统 Unix 工具。这些工具由于其以文本为中心的设计，通常提供更简洁的就地编辑语法。

在实现方面，需要注意的是 PowerShell 的方法涉及将整个文件读入内存，进行更改，然后写回。虽然这对于中等大小的文件效果很好，但对于非常大的文件来说，可能效率低下。在这种情况下，可能会考虑直接使用 `.NET` 方法，或求助于为流式处理大量数据设计的替代工具。

尽管有这些考虑因素，PowerShell 的灵活性和广泛的功能集，使其成为直接从命令行操纵文件的宝贵工具，特别是对于那些已经深入 Windows 生态系统或管理跨平台环境的人来说。
