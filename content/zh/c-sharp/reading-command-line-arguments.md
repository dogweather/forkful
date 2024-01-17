---
title:                "读取命令行参数"
html_title:           "C#: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##什么是命令行参数？为什么程序员要使用它？
命令行参数（Command Line Arguments）是指在运行一个程序时，通过命令行输入的参数。程序员使用它来为程序提供额外的信息，使程序更加灵活和可定制。

## 如何实现？
在C#中，可以使用System命名空间下的Environment类的GetCommandLineArgs方法来读取命令行参数。代码示例如下：
```
using System;

class Program
{
    static void Main(string[] args)
    {
        // 读取命令行参数
        string[] arguments = Environment.GetCommandLineArgs();

        // 输出参数
        foreach (string arg in arguments)
        {
            Console.WriteLine("Argument: " + arg);
        }
    }
}
```
假设我们运行该程序并输入命令行参数"Hello"和"World"，则输出结果如下：
```
Argument: Program.exe
Argument: Hello
Argument: World
```

## 深入探讨
命令行参数的使用可以追溯到早期的计算机操作系统，如Unix。目前，也有其他方式来为程序提供输入，如用户界面（User Interface）或配置文件（Config file）。但是命令行参数仍然是很多程序员首选的方式，因为它简单且易于使用。

##参考资料
- [C#命令行参数的使用](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netframework-4.7.2)
- [命令行参数介绍](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_argument)