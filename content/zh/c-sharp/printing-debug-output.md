---
title:                "打印调试输出"
aliases:
- zh/c-sharp/printing-debug-output.md
date:                  2024-01-20T17:52:16.658513-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
打印调试输出是显示程序运行数据的方法。程序员这么做是为了检查代码逻辑和跟踪变量，确保一切按计划工作。

## How to: (如何操作：)
在C#中打印调试信息，通常使用`Console.WriteLine()`。不复杂，看代码：

```C#
using System;

class DebugExample {
    static void Main() {
        int magicNumber = 42;
        // 这是输出语句
        Console.WriteLine("调试信息: 魔法数字是 " + magicNumber);
    }
}
```

运行结果会这样显示：

```
调试信息: 魔法数字是 42
```

## Deep Dive (深入了解)
早期，打印调试信息意味着将数据输出到打印机。现在，它指的是将数据显示在控制台或日志文件中。

替代方法包括使用调试器附加断点，或使用日志框架如log4net或NLog。

在C#中，除了`Console.WriteLine()`，还可以用`Debug.WriteLine()`输出调试信息，但它需要添加`System.Diagnostics`命名空间，并且只在调试版本中有效。

```C#
using System.Diagnostics;

class DebugExample {
    static void Main() {
        int magicNumber = 42;
        Debug.WriteLine("调试信息: 魔法数字是 " + magicNumber);
    }
}
```

## See Also (另请参阅)
- Microsoft C# 文档: [https://docs.microsoft.com/zh-cn/dotnet/csharp/](https://docs.microsoft.com/zh-cn/dotnet/csharp/)
- .NET 日志记录说明：[https://docs.microsoft.com/zh-cn/dotnet/core/extensions/logging?tabs=command-line](https://docs.microsoft.com/zh-cn/dotnet/core/extensions/logging?tabs=command-line)
- 关于`System.Diagnostics`的官方文档: [https://docs.microsoft.com/zh-cn/dotnet/api/system.diagnostics?view=netcore-3.1](https://docs.microsoft.com/zh-cn/dotnet/api/system.diagnostics?view=netcore-3.1)
