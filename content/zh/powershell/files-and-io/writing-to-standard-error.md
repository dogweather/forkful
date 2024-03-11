---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:13.962941-07:00
description: "\u5728 PowerShell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u6D89\u53CA\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u76F4\u63A5\
  \u53D1\u9001\u5230 stderr \u6D41\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\
  \u6D41\u660E\u786E\u533A\u5206\u3002\u8FD9\u79CD\u5206\u79BB\u5141\u8BB8\u5BF9\u811A\
  \u672C\u7684\u8F93\u51FA\u8FDB\u884C\u66F4\u7CBE\u786E\u7684\u63A7\u5236\uFF0C\u4F7F\
  \u5F00\u53D1\u4EBA\u5458\u80FD\u591F\u5C06\u6B63\u5E38\u548C\u9519\u8BEF\u6D88\u606F\
  \u5B9A\u5411\u5230\u4E0D\u540C\u7684\u76EE\u7684\u5730\uFF0C\u8FD9\u5BF9\u4E8E\u9519\
  \u8BEF\u5904\u7406\u548C\u65E5\u5FD7\u8BB0\u5F55\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-11T00:14:21.830197-06:00'
model: gpt-4-0125-preview
summary: "\u5728 PowerShell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u6D89\u53CA\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u76F4\u63A5\
  \u53D1\u9001\u5230 stderr \u6D41\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\
  \u6D41\u660E\u786E\u533A\u5206\u3002\u8FD9\u79CD\u5206\u79BB\u5141\u8BB8\u5BF9\u811A\
  \u672C\u7684\u8F93\u51FA\u8FDB\u884C\u66F4\u7CBE\u786E\u7684\u63A7\u5236\uFF0C\u4F7F\
  \u5F00\u53D1\u4EBA\u5458\u80FD\u591F\u5C06\u6B63\u5E38\u548C\u9519\u8BEF\u6D88\u606F\
  \u5B9A\u5411\u5230\u4E0D\u540C\u7684\u76EE\u7684\u5730\uFF0C\u8FD9\u5BF9\u4E8E\u9519\
  \u8BEF\u5904\u7406\u548C\u65E5\u5FD7\u8BB0\u5F55\u81F3\u5173\u91CD\u8981\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
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
