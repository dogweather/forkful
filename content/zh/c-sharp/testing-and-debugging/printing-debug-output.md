---
date: 2024-01-20 17:52:16.658513-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728C#\u4E2D\u6253\u5370\u8C03\
  \u8BD5\u4FE1\u606F\uFF0C\u901A\u5E38\u4F7F\u7528`Console.WriteLine()`\u3002\u4E0D\
  \u590D\u6742\uFF0C\u770B\u4EE3\u7801\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.770212-06:00'
model: gpt-4-1106-preview
summary: "\u5728C#\u4E2D\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\uFF0C\u901A\u5E38\u4F7F\
  \u7528`Console.WriteLine()`\u3002\u4E0D\u590D\u6742\uFF0C\u770B\u4EE3\u7801\uFF1A\
  ."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
