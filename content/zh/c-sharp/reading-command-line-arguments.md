---
title:    "C#: 阅读命令行参数"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么要读取命令行参数

当我们使用命令行来运行程序，有时可能需要提供一些额外的信息来改变程序的行为。这些信息就是命令行参数。通过读取命令行参数，我们可以实现一个更加灵活和个性化的程序。

## 如何读取命令行参数

在C#中，我们可以使用`args`数组来接收命令行参数。下面是一个示例代码：

```C#
static void Main(string[] args)
{
    if(args.Length < 2)
    {
        Console.WriteLine("请输入至少2个参数");
        return;
    }
    // args[0]为程序名称，之后的参数依次存储在args数组中
    Console.WriteLine("你的名字是：" + args[1]);
}
```

假设我们的程序名称为`HelloWorld.exe`，我们可以在命令行中使用以下命令来运行程序并传入参数：

```
HelloWorld.exe John
```

这样，程序就会输出`你的名字是：John`。除了使用`args`数组，我们也可以使用`Environment.GetCommandLineArgs()`方法来获取参数。

## 深入了解命令行参数

在实际开发中，读取命令行参数并不仅限于上面提到的简单示例。我们还可以使用其他方法来解析参数，例如使用第三方库来提供更多的功能。同时，也需要注意对参数的正确处理，避免出现不必要的错误。

## 同时参考

- [Microsoft docs - Command-line arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# Corner - Working with Command Line Arguments Using C#](https://www.c-sharpcorner.com/uploadfile/mahesh/working-with-command-line-arguments-in-C-Sharp/)