---
aliases:
- /zh/powershell/manipulating-files-with-cli-one-liners/
date: 2024-01-27 16:22:05.636095-07:00
description: "\u4F7F\u7528 PowerShell \u4E2D\u7684 CLI \u5355\u884C\u547D\u4EE4\u64CD\
  \u4F5C\u6587\u4EF6\uFF0C\u662F\u5173\u4E8E\u5982\u4F55\u76F4\u63A5\u4ECE\u547D\u4EE4\
  \u884C\u5FEB\u901F\u66F4\u6539\u3001\u79FB\u52A8\u6216\u83B7\u53D6\u6587\u4EF6\u6570\
  \u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6548\u7387\uFF1B\
  \u5B83\u6BD4\u5BFC\u822A GUI \u6216\u4E3A\u7B80\u5355\u4EFB\u52A1\u7F16\u5199\u5197\
  \u957F\u7684\u811A\u672C\u8981\u5FEB\u3002"
lastmod: 2024-02-18 23:08:59.327237
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 PowerShell \u4E2D\u7684 CLI \u5355\u884C\u547D\u4EE4\u64CD\u4F5C\
  \u6587\u4EF6\uFF0C\u662F\u5173\u4E8E\u5982\u4F55\u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\
  \u5FEB\u901F\u66F4\u6539\u3001\u79FB\u52A8\u6216\u83B7\u53D6\u6587\u4EF6\u6570\u636E\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6548\u7387\uFF1B\u5B83\
  \u6BD4\u5BFC\u822A GUI \u6216\u4E3A\u7B80\u5355\u4EFB\u52A1\u7F16\u5199\u5197\u957F\
  \u7684\u811A\u672C\u8981\u5FEB\u3002"
title: "\u4F7F\u7528\u547D\u4EE4\u884C\u4E00\u884C\u547D\u4EE4\u64CD\u4F5C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用 PowerShell 中的 CLI 单行命令操作文件，是关于如何直接从命令行快速更改、移动或获取文件数据。程序员这样做是为了效率；它比导航 GUI 或为简单任务编写冗长的脚本要快。

## 如何操作:

### 读取文件
要快速显示文件的内容，使用 `Get-Content` 命令：
```PowerShell
Get-Content .\example.txt
```

### 写入文件
要向文件中写入新内容，可以使用 `Set-Content`：
```PowerShell
Set-Content -Path .\example.txt -Value "Hello, PowerShell!"
```

### 追加到文件
可以使用 `Add-Content` 将数据追加到文件末尾而不擦除其内容：
```PowerShell
Add-Content -Path .\example.txt -Value "Adding this line."
```

### 复制文件
使用 `Copy-Item` 复制文件很简单：
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### 删除文件
要删除文件，只需使用 `Remove-Item`：
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### 在文件中搜索
使用 `Select-String` 在文件中搜索文本：
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### 组合命令
PowerShell 真正闪耀的是其使用管道链式连接命令的能力。以下是如何找到文件并将它们复制到新目录的方法：
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## 深入了解

历史上，PowerShell 被引入作为传统命令提示符在 Windows 中的更强大替代品，提供了前所未有的对系统内部和数据存储的访问。它结合了命令行的速度与脚本的灵活性，成为 Windows 系统管理员和开发人员不可或缺的工具。

PowerShell 用于文件操作的替代工具包括 Unix 基础工具，如 `sed`、`awk`、`grep`，以及 Linux 和 MacOS 用户的 `bash` 脚本。虽然这些工具非常强大，各有其优点，但 PowerShell 为 Windows 环境提供了深度整合。

PowerShell 的一个值得注意的方面是它的面向对象性质。与许多将一切视为字符串或字节流的脚本语言不同，PowerShell 直接与 .NET 对象一起工作。这意味着当你操作文件时，你正在使用提供大量属性和方法的丰富对象，使得复杂任务变得更加简单。

PowerShell 的一个弱点，特别是对于 Linux 和 MacOS 用户来说，是与 bash 脚本或使用 Unix 命令行工具相比，其被认为过于冗长。此外，PowerShell 与 Windows 的深度整合有时会使跨平台脚本变得有些挑战，尽管 PowerShell Core 的努力旨在有效地弥补这一差距。

不管它的弱点如何，PowerShell 的优势在于其强大的单行命令能力、集成的脚本环境，以及它提供的对 Windows 生态系统的全面访问，使其成为那些希望直接从命令行操作文件等更多内容的人的必备工具。
