---
date: 2024-01-20 17:41:52.144735-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) - **\u5386\u53F2\u80CC\u666F**: .NET\u63D0\
  \u4F9B\u4E86`System.Text.RegularExpressions`\u7C7B\u5E93\u81EA.NET Framework\u521D\
  \u59CB\u7248\u672C\u8D77\u3002\u6B63\u5219\u8868\u8FBE\u5F0F\u5F3A\u5927\u4E14\u7075\
  \u6D3B\uFF0C\u662F\u5904\u7406\u6587\u672C\u5E38\u7528\u7684\u5DE5\u5177\u3002 -\
  \ **\u9009\u62E9**:\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.957981-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) - **\u5386\u53F2\u80CC\u666F**: .NET\u63D0\u4F9B\
  \u4E86`System.Text.RegularExpressions`\u7C7B\u5E93\u81EA.NET Framework\u521D\u59CB\
  \u7248\u672C\u8D77\u3002\u6B63\u5219\u8868\u8FBE\u5F0F\u5F3A\u5927\u4E14\u7075\u6D3B\
  \uFF0C\u662F\u5904\u7406\u6587\u672C\u5E38\u7528\u7684\u5DE5\u5177\u3002 - **\u9009\
  \u62E9**: \u9664\u4E86\u6B63\u5219\u8868\u8FBE\u5F0F\uFF0C\u4F60\u8FD8\u53EF\u4EE5\
  \u4F7F\u7528`string.Replace`\u6216`StringBuilder`\u6765\u5220\u9664\u5B57\u7B26\uFF0C\
  \u7279\u522B\u662F\u5728\u7B80\u5355\u66FF\u6362\u7684\u573A\u666F\u4E0B\u3002\u4F46\
  \u8FD9\u4E9B\u65B9\u6CD5\u5728\u5904\u7406\u590D\u6742\u6A21\u5F0F\u65F6\u663E\u5F97\
  \u529B\u4E0D\u4ECE\u5FC3\u3002 - **\u5B9E\u73B0\u7EC6\u8282**: `Regex.Replace`\u63A5\
  \u6536\u4E09\u4E2A\u53C2\u6570\uFF1A\u6E90\u5B57\u7B26\u4E32\u3001\u6A21\u5F0F\u3001\
  \u4EE5\u53CA\u66FF\u6362\u5B57\u7B26\u4E32\u3002\u6A21\u5F0F\u4F9D\u636E\u6B63\u5219\
  \u8868\u8FBE\u5F0F\u89C4\u5219\u6765\u5339\u914D\u5B57\u7B26\uFF0C\u7136\u540E\u5C06\
  \u5339\u914D\u7684\u5B57\u7B26\u66FF\u6362\u4E3A\u7A7A\u5B57\u7B26\u4E32\uFF08\u5373\
  \u5220\u9664\uFF09\u3002"
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
