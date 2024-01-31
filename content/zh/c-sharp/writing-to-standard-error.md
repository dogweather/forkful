---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
标准错误(stream)是用来输出程序的错误信息。程序员用它来分离错误信息和正常输出，方便调试和日志记录。

## How to (如何做)：
```C#
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("出现错误：无效的操作。");
        Console.WriteLine("这是正常的输出。");
    }
}
```
输出示例：
```
这是正常的输出。
出现错误：无效的操作。
```
注意错误消息通常在命令行或日志文件中显示为红色。

## Deep Dive (深入了解)
历史上，把错误信息写到标准错误可以让用户关注重要的问题，保持标准输出的清晰。备选方式包括使用日志框架。实现细节方面，`Console.Error`是`System.IO.TextWriter`的实例，和标准输出`Console.Out`分开。

## See Also (另请参阅)
- [Console.Error 属性 - Microsoft Docs](https://docs.microsoft.com/zh-cn/dotnet/api/system.console.error)
- [了解 .NET 中的 logging - Microsoft Docs](https://docs.microsoft.com/zh-cn/aspnet/core/fundamentals/logging/?view=aspnetcore-6.0)
