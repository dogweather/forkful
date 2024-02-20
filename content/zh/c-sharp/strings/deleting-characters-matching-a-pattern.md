---
date: 2024-01-20 17:41:52.144735-07:00
description: "\u5728\u5904\u7406\u5B57\u7B26\u4E32\u65F6\uFF0C\u6211\u4EEC\u7ECF\u5E38\
  \u9700\u8981\u5220\u9664\u5339\u914D\u7279\u5B9A\u6A21\u5F0F\u7684\u5B57\u7B26\u3002\
  \u8FD9\u662F\u56E0\u4E3A\u6E05\u7406\u8F93\u5165\u6570\u636E\u3001\u63D0\u53D6\u5173\
  \u952E\u4FE1\u606F\u6216\u7B80\u5316\u6587\u672C\u5206\u6790\u4EFB\u52A1\u9700\u8981\
  \u79FB\u9664\u65E0\u5173\u6216\u4E0D\u9700\u8981\u7684\u5B57\u7B26\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.783585
model: gpt-4-1106-preview
summary: "\u5728\u5904\u7406\u5B57\u7B26\u4E32\u65F6\uFF0C\u6211\u4EEC\u7ECF\u5E38\
  \u9700\u8981\u5220\u9664\u5339\u914D\u7279\u5B9A\u6A21\u5F0F\u7684\u5B57\u7B26\u3002\
  \u8FD9\u662F\u56E0\u4E3A\u6E05\u7406\u8F93\u5165\u6570\u636E\u3001\u63D0\u53D6\u5173\
  \u952E\u4FE1\u606F\u6216\u7B80\u5316\u6587\u672C\u5206\u6790\u4EFB\u52A1\u9700\u8981\
  \u79FB\u9664\u65E0\u5173\u6216\u4E0D\u9700\u8981\u7684\u5B57\u7B26\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
在处理字符串时，我们经常需要删除匹配特定模式的字符。这是因为清理输入数据、提取关键信息或简化文本分析任务需要移除无关或不需要的字符。

## How to: (如何操作)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string pattern = "[^a-zA-Z0-9]"; // 定义模式，这里删除非字母和非数字字符
        string input = "Hello, 世界! 123.";
        string result = Regex.Replace(input, pattern, "");

        Console.WriteLine(result); // 输出: Hello123
    }
}
```
样例输出:
```
Hello123
```

## Deep Dive (深入了解)
- **历史背景**: .NET提供了`System.Text.RegularExpressions`类库自.NET Framework初始版本起。正则表达式强大且灵活，是处理文本常用的工具。
- **选择**: 除了正则表达式，你还可以使用`string.Replace`或`StringBuilder`来删除字符，特别是在简单替换的场景下。但这些方法在处理复杂模式时显得力不从心。
- **实现细节**: `Regex.Replace`接收三个参数：源字符串、模式、以及替换字符串。模式依据正则表达式规则来匹配字符，然后将匹配的字符替换为空字符串（即删除）。

## See Also (更多资源)
- [Regular Expressions (.NET) official documentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regex.Replace Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-6.0)
- [C# Guide by Microsoft](https://docs.microsoft.com/en-us/dotnet/csharp/)
