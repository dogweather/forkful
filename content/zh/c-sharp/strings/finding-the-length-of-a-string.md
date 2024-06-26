---
date: 2024-01-20 17:46:55.436936-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5B57\u7B26\u4E32`\"\u4F60\u597D\
  , \u4E16\u754C!\"`\u7684\u957F\u5EA6\u662F7\u3002`Length`\u5C5E\u6027\u8BA1\u7B97\
  \u51FA\u4E86\u5B57\u7B26\u7684\u6570\u91CF\u3002\u6CE8\u610F\uFF0C\u6C49\u5B57\u4E5F\
  \u88AB\u8BA1\u4E3A\u4E00\u4E2A\u5B57\u7B26\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.072733-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5B57\u7B26\u4E32`\"\u4F60\u597D, \u4E16\u754C\
  !\"`\u7684\u957F\u5EA6\u662F7\u3002`Length`\u5C5E\u6027\u8BA1\u7B97\u51FA\u4E86\u5B57\
  \u7B26\u7684\u6570\u91CF\u3002\u6CE8\u610F\uFF0C\u6C49\u5B57\u4E5F\u88AB\u8BA1\u4E3A\
  \u4E00\u4E2A\u5B57\u7B26\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (如何操作)
```C#
string greeting = "你好, 世界!";
int length = greeting.Length;
Console.WriteLine(length); // 输出: 7
```
字符串`"你好, 世界!"`的长度是7。`Length`属性计算出了字符的数量。注意，汉字也被计为一个字符。

## Deep Dive (深入探索)
早期的编程语言可能没有内建的字符串长度功能，需要手动遍历字符计数。在C#里，`.Length`属性给了我们一个快速、准确的方法来获取字符串长度。但是在处理有些编码（比如UTF-16）时，要注意一个逻辑上的字符可能由多个代码单元组成。除了`.Length`，其他技术，例如LINQ的`.Count()`方法，也能计算长度，但通常`.Length`是更直接、更快的选择。实现细节上，`.Length`返回的是一个`int`类型的数值，表示字符串中`Char`对象的数量。

## See Also (另请参阅)
- [Microsoft Docs on String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [Understanding Text in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding)
- [Strings (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
