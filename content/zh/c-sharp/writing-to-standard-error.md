---
title:                "标准错误的编写"
html_title:           "C#: 标准错误的编写"
simple_title:         "标准错误的编写"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 内容及原因
写入标准错误是一种程序员常用的技术，它允许将错误消息输出到控制台，而不是被隐藏在程序中。程序员使用它来帮助调试和查找程序中的错误。

## 如何操作
在C#中，使用 `Console.Error.WriteLine()` 方法来将错误消息输出到标准错误。这个方法接受一个字符串参数，可以是任何你想要输出的消息。接下来，让我们看一下具体的代码示例和输出。

```C#
try
{
    // some code that may throw an exception
}
catch (Exception ex)
{
    Console.Error.WriteLine("错误消息：" + ex.Message);
}
```

输出：

```
错误消息：无效的操作数。
```

## 深入探讨
写入标准错误最初是由Unix系统引入的，旨在通过控制台输出错误消息来帮助调试。如今，它已经成为许多编程语言的标准功能，包括C#。除了使用 `Console.Error.WriteLine()` 方法之外，还可以使用其他方法来实现写入标准错误，比如使用 `TraceListener` 类或 `EventLog` 类。

## 相关资料
想要了解更多关于写入标准错误的信息，可以参考以下资源：

- [Microsoft官方文档：Console类](https://docs.microsoft.com/zh-cn/dotnet/api/system.console?view=netcore-3.1)