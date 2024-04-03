---
date: 2024-01-20 17:45:11.143549-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.755260-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to: (怎么做：)
```C#
string fullText = "Hello, World! Welcome to the universe of C#.";
// 使用Substring方法起始索引和长度提取子串
string greeting = fullText.Substring(0, 13);
// 结果: "Hello, World!"
Console.WriteLine(greeting);

// 使用Substring方法起始索引提取到末尾的字符串
string context = fullText.Substring(14);
// 结果: "Welcome to the universe of C#."
Console.WriteLine(context);
```
输出：
```
Hello, World!
Welcome to the universe of C#.
```

## Deep Dive (深入探讨)
早期的C#版本中就有了字符串提取。这是基本的需求之一。除了`Substring`方法，我们还可以用`string`类型的索引器来拿单个字符，或者用`string.Split`方法来根据分隔符把字符串拆成数组再提取我们要的部分。C#是建立在.NET Framework上的。.NET 5开始，它就和.NET Core合并了，这让跨平台工作变得更简单。从C# 8开始，我们还有了更多的字符串处理功能，比如`Span<T>`和`Memory<T>`，这两个功能提供了访问和修改字符串的新方式，同时也提高了性能。

## See Also (另请参阅)
- C# Programming Guide - Strings (https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- .NET API Documentation - String.Substring (https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- .NET API Documentation - Memory<T> (https://docs.microsoft.com/en-us/dotnet/api/system.memory-1)
- .NET API Documentation - Span<T> (https://docs.microsoft.com/en-us/dotnet/api/system.span-1)
