---
title:                "C#: 读取命令行参数"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么读取命令行参数？

读取命令行参数是一种非常有用的技能。它允许程序员在运行程序时通过命令行输入不同的参数，以改变程序的行为。这样，程序就可以在不同的情况下运行，提高了灵活性和可重复性。

## 如何

要读取命令行参数，首先需要在程序中引入`System`命名空间。然后，在`Main`方法中，使用`args`参数来读取命令行输入的参数。例如：

```C#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            // 在命令行输入 "C# Mandarin"
            // args[0]为"C#"，args[1]为"Mandarin"
            Console.WriteLine($"Hello {args[0]} readers!");
            Console.WriteLine($"This article is written in {args[1]}. Enjoy!");
        }
    }
}
```

在命令行运行程序后，输出将会是：

```
Hello C# readers!
This article is written in Mandarin. Enjoy!
```

当然，这只是一个简单的例子。在实际应用中，我们还可以为命令行参数设置默认值，检查参数是否存在等。

## 深入了解

在命令行中输入参数时，我们可以使用空格、逗号或其他字符来分隔不同的参数。如果某个参数本身包含空格，可以用双引号括起来。此外，我们还可以使用特殊字符来表示选项或标志，如`-h`表示帮助信息，`-v`表示程序版本等。在程序中，我们可以通过`args.Length`来获取输入的参数数量，通过`args[i]`来获取第`i`个参数。更多关于命令行参数的信息，可以在[MDC命令行参数](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netcore-3.1)和[命令行参数解析器](https://github.com/commandlineparser/commandline)中深入了解。

## 参考资料

- [MDC命令行参数](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netcore-3.1)
- [命令行参数解析器](https://github.com/commandlineparser/commandline)