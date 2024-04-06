---
date: 2024-01-26 04:12:32.942047-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728C#\u73AF\u5883\u4E2D\u542F\u52A8\
  REPL\uFF0C\u53EF\u4EE5\u4F7F\u7528C#\u4EA4\u4E92\u5F0F\u7A97\u53E3\u6216\u5728\u7EC8\
  \u7AEF\u8FD0\u884C`dotnet-script`\u3002\u4E0B\u9762\u662F\u4F7F\u7528\u5B83\u7684\
  \u4E00\u4E2A\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.931978-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728C#\u73AF\u5883\u4E2D\u542F\u52A8REPL\uFF0C\
  \u53EF\u4EE5\u4F7F\u7528C#\u4EA4\u4E92\u5F0F\u7A97\u53E3\u6216\u5728\u7EC8\u7AEF\
  \u8FD0\u884C`dotnet-script`\u3002\u4E0B\u9762\u662F\u4F7F\u7528\u5B83\u7684\u4E00\
  \u4E2A\u793A\u4F8B\uFF1A."
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
