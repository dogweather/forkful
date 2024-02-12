---
title:                "写入标准错误"
aliases:
- /zh/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:32:44.854334-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
