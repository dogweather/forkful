---
title:                "将文档写入标准错误"
html_title:           "C#: 将文档写入标准错误"
simple_title:         "将文档写入标准错误"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么

在编写C#程序时，有时候我们需要输出一些错误信息来帮助我们调试代码。这就是为什么我们需要了解如何将信息写入标准错误流的原因。

# 如何做

通过使用标准错误流（standard error）来输出错误信息，可以让我们更轻松地调试和修复程序中的错误。下面是一些使用C#语言写入标准错误流的示例代码和输出：

```C#
try
{
   // 尝试执行某些代码，可能会产生错误
}
catch(Exception ex)
{
   // 将错误信息写入标准错误流
   Console.Error.WriteLine("发生错误：" + ex.Message);
}
```

输出：

```
发生错误：对象引用未设置到对象的实例。
```

```C#
int num1 = 10;
int num2 = 0;
try
{
   // 尝试除以0，会产生除数为0的错误
   int result = num1 / num2;
}
catch(DivideByZeroException ex)
{
   // 将错误信息写入标准错误流
   Console.Error.WriteLine("发生错误：" + ex.Message);
}
```

输出：

```
发生错误：尝试除以零。
```

# 深入探讨

标准错误流（standard error）是C#程序中一个很重要的概念，它可以让我们更轻松地调试程序中的错误。通过将错误信息写入标准错误流，我们可以在程序运行时实时地捕获并处理错误，从而更快地修复程序中的bug。

# 链接

- [MSDN文档：标准错误流](https://docs.microsoft.com/zh-cn/dotnet/api/system.console.error?view=net-5.0)
- [C# Guide：异常处理](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/exceptions/)