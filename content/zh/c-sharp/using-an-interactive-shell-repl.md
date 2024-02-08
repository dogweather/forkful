---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:12:32.942047-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
REPL，即读取-求值-打印循环，允许你输入C#代码并交互式地运行它。程序员用它来进行快速实验、调试或学习C#，无需设置完整项目的繁琐步骤。

## 如何操作：
在C#环境中启动REPL，可以使用C#交互式窗口或在终端运行`dotnet-script`。下面是使用它的一个示例：

```csharp
> var greeting = "Hello, REPL!";
> Console.WriteLine(greeting);
Hello, REPL!
>
```

你会立即得到反馈。无需编译和运行步骤。只需编码并查看结果。

## 深入探讨
REPL从Lisp走向现代语言，在像Python这样的动态语言中蓬勃发展。对于C#，Roslyn让REPL更接近开发者。对于Roslyn来说是`csi`，对于.NET Core则是`dotnet-script`，这两个都是坚实的选择。更深入一点：它们逐行评估代码，而不是一次性评估全部，这与典型的C#应用程序的执行模型有所不同。这影响到执行间状态的持久性和变量的作用域。

Visual Studio的C#交互式窗口是一个由Roslyn支持的REPL。它具有智能提示、多重引用和NuGet包支持。与早期的命令行实验相比，这是一个很大的进步。

对于其他语言，Python使用`IDLE`，JavaScript有Node.js的REPL，F#带有`F#交互式`。每种都促进了即时反馈循环，对于测试小代码片段或理解语言特性来说非常宝贵。

## 另请参阅
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
