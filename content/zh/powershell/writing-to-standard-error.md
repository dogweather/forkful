---
title:                "写入标准错误"
aliases:
- zh/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:13.962941-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在 PowerShell 中写入标准错误（stderr）涉及将错误信息或诊断信息直接发送到 stderr 流，与标准输出（stdout）流明确区分。这种分离允许对脚本的输出进行更精确的控制，使开发人员能够将正常和错误消息定向到不同的目的地，这对于错误处理和日志记录至关重要。

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
