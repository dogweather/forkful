---
date: 2024-01-20 17:38:04.467472-07:00
description: "\u628A\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u610F\u5473\u7740\
  \u5C06\u6240\u6709\u5927\u5199\u5B57\u7B26\u6539\u4E3A\u5C0F\u5199\u683C\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4EE5\u5B9E\u73B0\u6570\u636E\u4E00\u81F4\u6027\
  \u3001\u6BD4\u8F83\u64CD\u4F5C\uFF0C\u4EE5\u53CA\u6EE1\u8DB3\u5927\u5C0F\u5199\u4E0D\
  \u654F\u611F\u7684\u641C\u7D22\u9700\u6C42\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.752941-06:00'
model: gpt-4-1106-preview
summary: "\u628A\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u610F\u5473\u7740\
  \u5C06\u6240\u6709\u5927\u5199\u5B57\u7B26\u6539\u4E3A\u5C0F\u5199\u683C\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4EE5\u5B9E\u73B0\u6570\u636E\u4E00\u81F4\u6027\
  \u3001\u6BD4\u8F83\u64CD\u4F5C\uFF0C\u4EE5\u53CA\u6EE1\u8DB3\u5927\u5C0F\u5199\u4E0D\
  \u654F\u611F\u7684\u641C\u7D22\u9700\u6C42\u3002."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: (如何操作)
```C#
string originalText = "Hello, World!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText);  // 输出: hello, world!
```
简单的`ToLower()`方法就可以把字符串转换成全小写。

## Deep Dive (深入了解)
在.NET中，转换字符串到小写已被嵌入多年，一个重要的方法是`ToLower()`。不同地区的文本有不同的大小写规则，因此.NET还提供了`ToLowerInvariant()`，该方法不考虑区域性地来转换。还有一些替代方法，如LINQ：

```C#
string mixedCase = "C# Programming";
string lower = new string(mixedCase.Select(char.ToLower).ToArray());

Console.WriteLine(lower);  // 输出: c# programming
```

但是`ToLower()`方法因其简便性而被广泛使用。在将字符串转向小写时要注意，一些文化区域对字母的大小写转换有特殊规则，可能不符合你的预期。所以在全球化应用程序中需要特别注意这点。

## See Also (另请参阅)
- Microsoft Docs - String.ToLower: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- Microsoft Docs - String.ToLowerInvariant: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant
- Stack Overflow - Case Insensitive String Operations: https://stackoverflow.com/questions/444798/case-insensitive-string-comparison-in-c-sharp
