---
title:                "日志记录"
date:                  2024-01-26T01:01:18.098872-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/logging.md"
---

{{< edit_this_page >}}

## 什么是日志记录以及为什么要使用？
日志记录是在运行时记录应用程序事件和数据输出的过程。程序员通过日志记录来诊断bug、监控软件性能、追踪用户操作，并维护与安全和业务标准的符合性。

## 如何进行日志记录：
在C#中，你可以使用内置的`System.Diagnostics`命名空间或者第三方库，如NLog或log4net。这里是一个使用.NET Core中可用的`ILogger`接口的简短示例：

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("这是一条信息性消息。");
        logger.LogWarning("这是一条警告消息。");
        logger.LogError("这是一条错误消息。");
    }
}
```

示例输出：
```
info: Program[0]
      这是一条信息性消息。
warn: Program[0]
      这是一条警告消息。
fail: Program[0]
      这是一条错误消息。
```

## 深入了解
在软件开发中，日志记录的历史几乎和编程本身一样久；从简单的打印语句发展到了复杂的、可配置的系统。最初，日志记录是通过写入文件或控制台来完成的，但这已经发展到包括更复杂的结构，如日志聚合系统和分布式追踪平台（如ELK栈或Jaeger）。

与.NET内置日志记录的替代方案包括第三方库：
- **NLog**：多功能且易于设置，具有大量的日志路由、格式化和过滤特性。
- **log4net**：受到Java log4j库的启发，它可以通过XML进行高度配置，并支持多种日志仓储。

当涉及到实现细节时，你选择的日志抽象化（如Microsoft.Extensions.Logging）和底层日志提供者可以显著影响你的应用程序的性能和可靠性。适当配置日志级别并确保写日志不会成为瓶颈是至关重要的。

此外，结构化日志记录——不只是记录字符串，而是记录键值对或对象——允许更精确和可操作的日志，这些日志更容易查询和分析。

## 另请参阅
- [Microsoft.Extensions.Logging 文档](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog 文档](https://nlog-project.org/documentation/)
- [log4net 文档](https://logging.apache.org/log4net/)
- [Serilog 文档](https://serilog.net/)（结构化日志记录的一个例子）
