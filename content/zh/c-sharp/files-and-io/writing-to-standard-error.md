---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:44.854334-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u5728 C# \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528 `Console.Error` \u6D41\u6765\u5B9E\u73B0\u5411\u6807\u51C6\u9519\u8BEF\u5199\
  \u5165\u3002\u8FD9\u4E2A\u6D41\u4E13\u95E8\u7528\u4E8E\u9519\u8BEF\u6D88\u606F\u548C\
  \u8BCA\u65AD\u4FE1\u606F\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.785978-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528 `Console.Error` \u6D41\u6765\
  \u5B9E\u73B0\u5411\u6807\u51C6\u9519\u8BEF\u5199\u5165\u3002\u8FD9\u4E2A\u6D41\u4E13\
  \u95E8\u7528\u4E8E\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u3002\u8FD9\
  \u91CC\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
