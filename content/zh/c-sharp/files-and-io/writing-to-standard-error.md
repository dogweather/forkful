---
aliases:
- /zh/c-sharp/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:44.854334-07:00
description: "\u5728 C# \u4E2D\u5411\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u5199\
  \u5165\u5185\u5BB9\uFF0C\u6D89\u53CA\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\
  \u4FE1\u606F\u4E0E\u5E38\u89C4\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4EE5\
  \u5E2E\u52A9\u7528\u6237\u548C\u5F00\u53D1\u4EBA\u5458\u533A\u5206\u6B63\u5E38\u7A0B\
  \u5E8F\u8F93\u51FA\u548C\u9519\u8BEF\u901A\u77E5\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4F7F\u8C03\u8BD5\u548C\u8BB0\u5F55\u66F4\u52A0\u9AD8\u6548\
  \uFF0C\u8BA9\u5E94\u7528\u7684\u64CD\u4F5C\u548C\u7EF4\u62A4\u66F4\u52A0\u987A\u7545\
  \u3002"
lastmod: 2024-02-18 23:08:59.147008
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\u5411\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u5199\u5165\
  \u5185\u5BB9\uFF0C\u6D89\u53CA\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\
  \u606F\u4E0E\u5E38\u89C4\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4EE5\u5E2E\
  \u52A9\u7528\u6237\u548C\u5F00\u53D1\u4EBA\u5458\u533A\u5206\u6B63\u5E38\u7A0B\u5E8F\
  \u8F93\u51FA\u548C\u9519\u8BEF\u901A\u77E5\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u4F7F\u8C03\u8BD5\u548C\u8BB0\u5F55\u66F4\u52A0\u9AD8\u6548\uFF0C\
  \u8BA9\u5E94\u7528\u7684\u64CD\u4F5C\u548C\u7EF4\u62A4\u66F4\u52A0\u987A\u7545\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 C# 中向标准错误（stderr）写入内容，涉及将错误消息和诊断信息与常规输出（stdout）分开，以帮助用户和开发人员区分正常程序输出和错误通知。程序员这样做是为了使调试和记录更加高效，让应用的操作和维护更加顺畅。

## 如何实现：
在 C# 中，可以使用 `Console.Error` 流来实现向标准错误写入。这个流专门用于错误消息和诊断信息。这里是一个基本示例：

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

示例输出（到 stderr）：
```
Error: Failed to process the request.
```

在您可能使用提供高级日志记录能力的第三方库（如 `Serilog` 或 `NLog`）的场景中，您可以配置这些库将错误日志写入 stderr。虽然这些示例集中在简单的控制台重定向上，但请记住，在生产应用程序中，日志框架提供了更加健壮的错误处理和输出选项。这里是一个使用 `Serilog` 的简单示例：

首先，安装 Serilog 包及其控制台接收器：

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

然后，配置 Serilog 写入 stderr：

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

示例输出（对于错误消息，到 stderr）：
```
[15:04:20 ERR] This is an error message.
```

注意：Serilog 控制台接收器中的 `standardErrorFromLevel` 配置将所有在指定级别（在此例中为 Error）或更高级别的日志事件重定向到标准错误流，而像信息这样的低级消息则被写入到标准输出流。
