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

## 为什么要读取命令行参数？

在编程过程中，有时候我们需要从命令行中读取用户输入的参数来实现特定的功能。读取命令行参数可以帮助程序与用户进行交互并且根据不同的输入来执行不同的操作。

## 如何获取命令行参数

在C#中，我们可以通过使用`args`这个参数来访问命令行参数。这个参数是一个字符串数组，其中包含用户在命令行中输入的所有参数。我们可以使用循环来遍历这个数组并对每个参数进行处理。

```C#
// 遍历args数组
foreach (string arg in args)
{
    // 对每个参数进行处理
    // 例如打印参数到控制台
    Console.WriteLine(arg);
}
```

如果我们想要获取特定位置的参数，可以通过索引来访问数组中的元素。第一个参数的索引值为0，依次类推。

```C#
// 获取第一个参数
string firstArg = args[0];
```

我们也可以使用`Contains()`方法来检查用户是否输入了特定的参数。

```C#
// 检查用户是否输入了"-h"这个参数
if (args.Contains("-h"))
{
    Console.WriteLine("这个参数会显示帮助信息");
}
```

## 深入探讨

除了上述提到的方法，我们还可以使用`GetCommandLineArgs()`方法来获取命令行参数。这个方法会返回一个包含所有参数的字符串数组。另外，在C# 7.1及以上版本中，我们还可以使用新的`Main(string[])`方法来直接获取命令行参数，不需要使用`args`这个参数。

除了读取命令行参数，我们也可以在命令行中传递特定的标志来指示程序执行不同的操作。这个标志可以通过在参数前加上`/`或`-`来表示，并且可以带有一个值。

```C#
// /m参数后面跟着一个值
/m:10
```

读取命令行参数是编写交互式和灵活的程序的重要部分。通过结合使用字符串处理的技巧，我们可以轻松地实现对命令行参数的处理和解析。

## 参考链接

- [MSDN官方文档：使用命令行参数](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [通过命令行参数设置.NET Core控制台应用的行为和配置](https://docs.microsoft.com/zh-cn/dotnet/core/tutorials/cli-arguments)