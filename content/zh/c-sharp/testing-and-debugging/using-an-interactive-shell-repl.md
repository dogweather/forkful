---
date: 2024-01-26 04:12:32.942047-07:00
description: "REPL\uFF0C\u5373\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u5141\u8BB8\u4F60\u8F93\u5165C#\u4EE3\u7801\u5E76\u4EA4\u4E92\u5F0F\u5730\u8FD0\
  \u884C\u5B83\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u8FDB\u884C\u5FEB\u901F\u5B9E\
  \u9A8C\u3001\u8C03\u8BD5\u6216\u5B66\u4E60C#\uFF0C\u65E0\u9700\u8BBE\u7F6E\u5B8C\
  \u6574\u9879\u76EE\u7684\u7E41\u7410\u6B65\u9AA4\u3002"
lastmod: '2024-03-13T22:44:47.769254-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF0C\u5373\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u5141\u8BB8\u4F60\u8F93\u5165C#\u4EE3\u7801\u5E76\u4EA4\u4E92\u5F0F\u5730\u8FD0\
  \u884C\u5B83\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u8FDB\u884C\u5FEB\u901F\u5B9E\
  \u9A8C\u3001\u8C03\u8BD5\u6216\u5B66\u4E60C#\uFF0C\u65E0\u9700\u8BBE\u7F6E\u5B8C\
  \u6574\u9879\u76EE\u7684\u7E41\u7410\u6B65\u9AA4\u3002."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
