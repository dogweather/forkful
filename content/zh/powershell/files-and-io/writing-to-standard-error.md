---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:13.962941-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell \u901A\u8FC7\u4F7F\u7528 `Write-Error`\
  \ cmdlet \u6216\u5C06\u8F93\u51FA\u5B9A\u5411\u5230 `$host.ui.WriteErrorLine()`\
  \ \u65B9\u6CD5\u6765\u7B80\u5316\u5199\u5165 stderr \u7684\u8FC7\u7A0B\u3002\u7136\
  \u800C\uFF0C\u5BF9\u4E8E\u76F4\u63A5\u7684 stderr \u91CD\u5B9A\u5411\uFF0C\u4F60\
  \u53EF\u80FD\u66F4\u503E\u5411\u4E8E\u4F7F\u7528 .NET \u65B9\u6CD5\u6216 PowerShell\u2026"
lastmod: '2024-03-13T22:44:48.032579-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u901A\u8FC7\u4F7F\u7528 `Write-Error` cmdlet \u6216\u5C06\u8F93\
  \u51FA\u5B9A\u5411\u5230 `$host.ui.WriteErrorLine()` \u65B9\u6CD5\u6765\u7B80\u5316\
  \u5199\u5165 stderr \u7684\u8FC7\u7A0B\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u76F4\
  \u63A5\u7684 stderr \u91CD\u5B9A\u5411\uFF0C\u4F60\u53EF\u80FD\u66F4\u503E\u5411\
  \u4E8E\u4F7F\u7528 .NET \u65B9\u6CD5\u6216 PowerShell \u672C\u8EAB\u63D0\u4F9B\u7684\
  \u6587\u4EF6\u63CF\u8FF0\u7B26\u91CD\u5B9A\u5411."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作：
PowerShell 通过使用 `Write-Error` cmdlet 或将输出定向到 `$host.ui.WriteErrorLine()` 方法来简化写入 stderr 的过程。然而，对于直接的 stderr 重定向，你可能更倾向于使用 .NET 方法或 PowerShell 本身提供的文件描述符重定向。

**示例 1:** 使用 `Write-Error` 将错误消息写入 stderr。

```powershell
Write-Error "这是一个错误消息。"
```

输出到 stderr:
```
Write-Error: 这是一个错误消息。
```

**示例 2:** 使用 `$host.ui.WriteErrorLine()` 直接写入 stderr。

```powershell
$host.ui.WriteErrorLine("直接写入 stderr。")
```

输出到 stderr:
```
直接写入 stderr。
```

**示例 3:** 使用 .NET 方法写入 stderr。

```powershell
[Console]::Error.WriteLine("使用 .NET 方法写入 stderr")
```

此方法的输出:
```
使用 .NET 方法写入 stderr
```

**示例 4:** 使用文件描述符 `2>` 重定向错误输出。

PowerShell 中的文件描述符可以重定向不同的流。对于 stderr，文件描述符是 `2`。这是一个例子，展示了在执行生成错误的命令时将 stderr 重定向到名为 `error.log` 的文件。

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

这个示例没有产生控制台输出，但在当前目录生成了一个包含尝试访问不存在的文件的错误消息的 `error.log` 文件。

总之，PowerShell 提供了多种方法来有效地编写和管理错误输出，允许在脚本和应用程序中采用复杂的错误处理和日志记录策略。
