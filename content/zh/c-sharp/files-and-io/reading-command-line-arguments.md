---
date: 2024-01-20 17:55:32.101720-07:00
description: "How to (\u600E\u4E48\u505A) \u547D\u4EE4\u884C\u53C2\u6570\u662F\u6700\
  \u53E4\u8001\u7684\u53C2\u6570\u4F20\u9012\u65B9\u5F0F\u4E4B\u4E00\uFF0C\u5B83\u8BA9\
  \u65E9\u671F\u7684\u8F6F\u4EF6\u80FD\u5728\u6CA1\u6709\u56FE\u5F62\u754C\u9762\u7684\
  \u60C5\u51B5\u4E0B\u8FDB\u884C\u4EA4\u4E92\u3002\u5B83\u4F9D\u7136\u5728\u8BB8\u591A\
  \u60C5\u5F62\u4E0B\u975E\u5E38\u6709\u7528\uFF0C\u5C24\u5176\u662F\u5728\u7F16\u5199\
  \u811A\u672C\u548C\u81EA\u52A8\u5316\u5DE5\u5177\u65F6\u3002 \u9664\u4E86\u76F4\u63A5\
  \u4F7F\u7528 `args` \u6570\u7EC4\u4E4B\u5916\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u4F7F\
  \u7528 `System.Environment` \u7684\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.993632-06:00'
model: gpt-4-1106-preview
summary: "\u9664\u4E86\u76F4\u63A5\u4F7F\u7528 `args` \u6570\u7EC4\u4E4B\u5916\uFF0C\
  \u4F60\u8FD8\u53EF\u4EE5\u4F7F\u7528 `System.Environment` \u7684 `GetCommandLineArgs`\
  \ \u65B9\u6CD5\uFF0C\u6216\u8005\u7B2C\u4E09\u65B9\u5E93\u4F8B\u5982 `CommandLineParser`\
  \ \u6765\u5904\u7406\u66F4\u590D\u6742\u7684\u547D\u4EE4\u884C\u53C2\u6570\u89E3\
  \u6790\u3002"
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
