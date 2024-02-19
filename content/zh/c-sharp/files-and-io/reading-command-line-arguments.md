---
aliases:
- /zh/c-sharp/reading-command-line-arguments/
date: 2024-01-20 17:55:32.101720-07:00
description: "\u5728C#\u7A0B\u5E8F\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u53EF\u4EE5\u8BA9\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u63A5\u6536\u7528\u6237\u8F93\
  \u5165\u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\u914D\u7F6E\u9009\u9879\u6216\u8005\u6587\
  \u4EF6\u8DEF\u5F84\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u63D0\
  \u4F9B\u7075\u6D3B\u6027\u548C\u7528\u6237\u5B9A\u5236\u7684\u53EF\u80FD\u6027\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.146132
model: gpt-4-1106-preview
summary: "\u5728C#\u7A0B\u5E8F\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u53EF\
  \u4EE5\u8BA9\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u63A5\u6536\u7528\u6237\u8F93\u5165\
  \u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\u914D\u7F6E\u9009\u9879\u6216\u8005\u6587\u4EF6\
  \u8DEF\u5F84\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u63D0\u4F9B\
  \u7075\u6D3B\u6027\u548C\u7528\u6237\u5B9A\u5236\u7684\u53EF\u80FD\u6027\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
在C#程序中读取命令行参数可以让你的应用程序接收用户输入的信息，比如配置选项或者文件路径。程序员这么做是为了提供灵活性和用户定制的可能性。

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
