---
title:    "C#: 打印调试输出"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么：了解为什么要打印调试输出

既然作为程序员，我们不可避免地遇到各种各样的bug，那么如何通过打印调试输出来帮助我们更好地找出并解决这些问题呢？让我们来看看为什么打印调试输出是一个有用的工具。

## 如何：使用C#编写打印调试输出的代码示例

```C#
Console.WriteLine("Hello World!"); // 打印普通文本
Console.WriteLine("The value of x is: " + x); // 打印变量的值
Console.WriteLine($"The value of y is: {y}"); // 使用插值表达式打印变量的值
Debug.WriteLine("The program reached this point."); // 打印调试文本
```

下面是上述代码的输出结果：

```
Hello World!
The value of x is: 10
The value of y is: 5
```

使用打印调试输出可以让我们快速确认程序是否正常运行，同时也可以输出变量的值，帮助我们更好地理解程序的执行过程。除了上述代码示例中的几种方法，C#还有许多其他的打印调试输出的方式。

## 深入了解打印调试输出

除了上面提到的打印普通文本、变量以及调试文本外，还有几种有用的打印调试输出的方式。比如，我们可以使用条件断点来在特定的条件下打印调试输出。我们也可以使用`Trace`类来实时跟踪程序的执行过程，并将输出结果保存在日志文件中。

此外，打印调试输出也非常有用于调试异步代码。使用`Task.Delay()`方法可以在代码中加入延迟，从而让我们有足够的时间来打印调试输出，以便更好地了解程序的执行情况。

## 查看更多

了解打印调试输出的更多方式，请参考以下链接：

- [C#打印调试输出文档](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/how-to-write-to-a-text-file)
- [使用条件断点调试 C# 代码](http://codestory.me/why-dont-you-use-conditional-breakpoint/)
- [使用Trace跟踪C#代码](https://www.c-sharpcorner.com/article/trace-in-c-sharp/)
- [在异步代码中使用调试输出](https://www.codingame.com/playgrounds/4241/your-ultimate-async-await-tutorial-in-c/taskdelay)

## 参阅

- [Markdown基础知识教程](https://coding.net/help/doc/project/markdown.html)
- [C#官方文档](https://docs.microsoft.com/zh-cn/dotnet/csharp/)