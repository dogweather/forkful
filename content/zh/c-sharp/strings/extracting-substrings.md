---
date: 2024-01-20 17:45:11.143549-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u65E9\u671F\u7684C#\u7248\u672C\u4E2D\
  \u5C31\u6709\u4E86\u5B57\u7B26\u4E32\u63D0\u53D6\u3002\u8FD9\u662F\u57FA\u672C\u7684\
  \u9700\u6C42\u4E4B\u4E00\u3002\u9664\u4E86`Substring`\u65B9\u6CD5\uFF0C\u6211\u4EEC\
  \u8FD8\u53EF\u4EE5\u7528`string`\u7C7B\u578B\u7684\u7D22\u5F15\u5668\u6765\u62FF\
  \u5355\u4E2A\u5B57\u7B26\uFF0C\u6216\u8005\u7528`string.Split`\u65B9\u6CD5\u6765\
  \u6839\u636E\u5206\u9694\u7B26\u628A\u5B57\u7B26\u4E32\u62C6\u6210\u6570\u7EC4\u518D\
  \u63D0\u53D6\u6211\u4EEC\u8981\u7684\u90E8\u5206\u3002C#\u662F\u5EFA\u7ACB\u5728\
  .NET\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.070602-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u65E9\u671F\u7684C#\u7248\u672C\u4E2D\u5C31\u6709\
  \u4E86\u5B57\u7B26\u4E32\u63D0\u53D6\u3002\u8FD9\u662F\u57FA\u672C\u7684\u9700\u6C42\
  \u4E4B\u4E00\u3002\u9664\u4E86`Substring`\u65B9\u6CD5\uFF0C\u6211\u4EEC\u8FD8\u53EF\
  \u4EE5\u7528`string`\u7C7B\u578B\u7684\u7D22\u5F15\u5668\u6765\u62FF\u5355\u4E2A\
  \u5B57\u7B26\uFF0C\u6216\u8005\u7528`string.Split`\u65B9\u6CD5\u6765\u6839\u636E\
  \u5206\u9694\u7B26\u628A\u5B57\u7B26\u4E32\u62C6\u6210\u6570\u7EC4\u518D\u63D0\u53D6\
  \u6211\u4EEC\u8981\u7684\u90E8\u5206\u3002C#\u662F\u5EFA\u7ACB\u5728.NET Framework\u4E0A\
  \u7684\u3002.NET 5\u5F00\u59CB\uFF0C\u5B83\u5C31\u548C.NET Core\u5408\u5E76\u4E86\
  \uFF0C\u8FD9\u8BA9\u8DE8\u5E73\u53F0\u5DE5\u4F5C\u53D8\u5F97\u66F4\u7B80\u5355\u3002\
  \u4ECEC# 8\u5F00\u59CB\uFF0C\u6211\u4EEC\u8FD8\u6709\u4E86\u66F4\u591A\u7684\u5B57\
  \u7B26\u4E32\u5904\u7406\u529F\u80FD\uFF0C\u6BD4\u5982`Span<T>`\u548C`Memory<T>`\uFF0C\
  \u8FD9\u4E24\u4E2A\u529F\u80FD\u63D0\u4F9B\u4E86\u8BBF\u95EE\u548C\u4FEE\u6539\u5B57\
  \u7B26\u4E32\u7684\u65B0\u65B9\u5F0F\uFF0C\u540C\u65F6\u4E5F\u63D0\u9AD8\u4E86\u6027\
  \u80FD\u3002"
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
