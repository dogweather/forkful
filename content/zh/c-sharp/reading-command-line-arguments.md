---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

命令行参数的读取是指从控制台程序获取输入的过程。程序员使用它来创建更灵活和响应用户需求的程序。

## 如何做：

下面的代码示例展示了在C#中如何读取命令行参数：

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine("Arg[{0}] = [{1}]", i, args[i]);
        }
    }
}
```

假如你在控制台输入以下命令：

```C#
dotnet run arg1 arg2 arg3
```

那么输出结果将会是：

```C#
Arg[0] = [arg1]
Arg[1] = [arg2]
Arg[2] = [arg3]
```

## 深入探讨：

在早期的命令行程序中，命令行参数被广泛地用于读取用户在命令行中输入的信息。在现代编程中，尽管有更人性化的用户界面，但命令行参数的使用仍然很常见。

现代的C#提供了很多解析命令行参数的工具库，例如CommandLineParser库，它可以帮助你更容易地处理复杂的参数模式。

原生地，C#在程序的Main函数中获取命令行参数。这些参数被存储在一个字符串数组中，程序启动时，`.NET runtime`会自动填充这个数组。

## 参考资料：

- [C# 101: Introduction to C# & .NET Core](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/intro-to-csharp/)
- [CommandLineParser Library](https://github.com/commandlineparser/commandline)