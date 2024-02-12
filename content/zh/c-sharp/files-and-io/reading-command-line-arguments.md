---
title:                "读取命令行参数"
aliases:
- zh/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:32.101720-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
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
