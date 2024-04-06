---
date: 2024-01-20 17:41:52.144735-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u6837\u4F8B\u8F93\u51FA."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.065727-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

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
