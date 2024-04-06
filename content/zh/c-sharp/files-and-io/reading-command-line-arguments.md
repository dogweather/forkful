---
date: 2024-01-20 17:55:32.101720-07:00
description: "How to (\u600E\u4E48\u505A) \u7B80\u5355\u4F8B\u5B50\u7528\u6765\u5C55\
  \u793A\u5982\u4F55\u83B7\u53D6\u547D\u4EE4\u884C\u53C2\u6570\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.947411-06:00'
model: gpt-4-1106-preview
summary: "How to (\u600E\u4E48\u505A) \u7B80\u5355\u4F8B\u5B50\u7528\u6765\u5C55\u793A\
  \u5982\u4F55\u83B7\u53D6\u547D\u4EE4\u884C\u53C2\u6570\uFF1A."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## How to (怎么做)
简单例子用来展示如何获取命令行参数：

```C#
using System;

class CommandLineArguments
{
    static void Main(string[] args)
    {
        Console.WriteLine("Argument count: " + args.Length);
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument {i}: {args[i]}");
        }
    }
}
```

假设编译后的程序名为 MyApp.exe，并用以下命令运行：

```
MyApp.exe firstArg secondArg "the third arg"
```

输出将会是：

```
Argument count: 3
Argument 0: firstArg
Argument 1: secondArg
Argument 2: the third arg
```

## Deep Dive (深入探讨)
命令行参数是最古老的参数传递方式之一，它让早期的软件能在没有图形界面的情况下进行交互。它依然在许多情形下非常有用，尤其是在编写脚本和自动化工具时。

除了直接使用 `args` 数组之外，你还可以使用 `System.Environment` 的 `GetCommandLineArgs` 方法，或者第三方库例如 `CommandLineParser` 来处理更复杂的命令行参数解析。

实现细节方面，当你的应用程序启动时，操作系统会把命令行参数作为字符串数组传递给 `Main` 方法。你可以通过下标访问每个参数。

## See Also (延伸阅读)
- [Microsoft's official documentation on command-line arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [CommandLineParser Library on GitHub](https://github.com/commandlineparser/commandline)
- [Environment.GetCommandLineArgs Method](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs)
