---
title:                "C#: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读命令行参数

命令行参数是指在运行程序时，通过命令行给程序传递一些参数，从而影响程序的运行结果。阅读命令行参数可以帮助程序员在开发过程中更加灵活地控制程序的行为，同时也能提高程序的可扩展性。因此，阅读命令行参数是开发人员非常重要的一项技能。

# 如何阅读命令行参数

阅读命令行参数的过程非常简单，只需要使用C#语言提供的相关类和方法即可。首先，在程序中添加`using System`语句，然后创建一个`Main`方法作为程序的入口。接下来，使用`Environment.GetCommandLineArgs()`方法可以获取到用户在命令行中输入的所有参数。然后，可以通过循环遍历参数数组来获取每个参数的值。下面是一个简单的示例代码：

```C#
using System;
class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("传递的命令行参数如下：");
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine(args[i]);
        }
    }
}
```

假设将以上代码保存为`Program.cs`并编译执行，在命令行中输入`dotnet Program.cs Hello World`，则会输出如下结果：

```
传递的命令行参数如下：
Hello
World
```

# 深入阅读命令行参数

当我们需要处理更加复杂的命令行参数时，可以使用`System.CommandLine`这个第三方库来帮助我们。它提供了更加方便的方式来解析和处理命令行参数，同时还支持自动生成帮助信息。下面是一个简单的示例代码，演示如何使用`System.CommandLine`来解析参数并输出结果：

```C#
using System;
using System.CommandLine;
using System.CommandLine.Invocation;

class Program
{
    static void Main(string[] args)
    {
        var rootCommand = new RootCommand
        {
            new Argument<string>("message"),
            new Argument<string>("name")
        };

        rootCommand.Handler = CommandHandler.Create((string message, string name) =>
        {
            Console.WriteLine($"{message}, {name}!");
        });

        rootCommand.Invoke(args);
    }
}
```

假设将以上代码保存为`Program.cs`并编译执行，在命令行中输入`dotnet Program.cs "Hello" --name "World"`，则会输出如下结果：

```
Hello, World!
```

总的来说，阅读和处理命令行参数是C#开发中必不可少的技能，希望本文能够帮助到大家。

# 参考链接
- [C#入门教程](https://www.runoob.com/csharp/csharp-tutorial.html)
- [System namespace](https://docs.microsoft.com/en-us/dotnet/api/system)
- [System.CommandLine namespace](https://docs.microsoft.com/en-us/dotnet/api/system.commandline)