---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:23.895475-07:00
description: "C#\u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u662F\u4E00\
  \u79CD\u5F3A\u5927\u7684\u5DE5\u5177\uFF0C\u7528\u4E8E\u5728\u5B57\u7B26\u4E32\u4E2D\
  \u8FDB\u884C\u6A21\u5F0F\u5339\u914D\uFF0C\u5141\u8BB8\u7A0B\u5E8F\u5458\u6709\u6548\
  \u5730\u641C\u7D22\u3001\u66FF\u6362\u3001\u5206\u5272\u6216\u63D0\u53D6\u6570\u636E\
  \u3002\u7A0B\u5E8F\u5458\u5229\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u8FDB\u884C\u4ECE\
  \u7B80\u5355\u7684\u9A8C\u8BC1\uFF08\u5982\u7535\u5B50\u90AE\u4EF6\u683C\u5F0F\u68C0\
  \u67E5\uFF09\u5230\u590D\u6742\u6587\u672C\u5904\u7406\u4EFB\u52A1\u7684\u5DE5\u4F5C\
  \uFF0C\u56E0\u4E3A\u5B83\u7684\u7075\u6D3B\u6027\u548C\u6027\u80FD\u3002"
lastmod: '2024-03-13T22:44:47.756554-06:00'
model: gpt-4-0125-preview
summary: "C#\u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u662F\u4E00\
  \u79CD\u5F3A\u5927\u7684\u5DE5\u5177\uFF0C\u7528\u4E8E\u5728\u5B57\u7B26\u4E32\u4E2D\
  \u8FDB\u884C\u6A21\u5F0F\u5339\u914D\uFF0C\u5141\u8BB8\u7A0B\u5E8F\u5458\u6709\u6548\
  \u5730\u641C\u7D22\u3001\u66FF\u6362\u3001\u5206\u5272\u6216\u63D0\u53D6\u6570\u636E\
  \u3002\u7A0B\u5E8F\u5458\u5229\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u8FDB\u884C\u4ECE\
  \u7B80\u5355\u7684\u9A8C\u8BC1\uFF08\u5982\u7535\u5B50\u90AE\u4EF6\u683C\u5F0F\u68C0\
  \u67E5\uFF09\u5230\u590D\u6742\u6587\u672C\u5904\u7406\u4EFB\u52A1\u7684\u5DE5\u4F5C\
  \uFF0C\u56E0\u4E3A\u5B83\u7684\u7075\u6D3B\u6027\u548C\u6027\u80FD\u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
---

{{< edit_this_page >}}

## 什么和为什么？
C#中的正则表达式（regex）是一种强大的工具，用于在字符串中进行模式匹配，允许程序员有效地搜索、替换、分割或提取数据。程序员利用正则表达式进行从简单的验证（如电子邮件格式检查）到复杂文本处理任务的工作，因为它的灵活性和性能。

## 如何操作：

### 简单模式匹配
要检查字符串是否包含特定模式，您可以使用`System.Text.RegularExpressions`命名空间中的`Regex.IsMatch`方法。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // 输出：True
    }
}
```

### 提取数据
使用正则表达式中的组从字符串提取数据可以通过`Regex.Match`方法完成。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // 输出：Year: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // 输出：Month: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // 输出：Day: 12
        }
    }
}
```

### 替换文本
`Regex.Replace`方法允许您替换字符串中与指定模式匹配的文本。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // 输出：Visit Google!
    }
}
```

### 分割字符串
您可以使用`Regex.Split`方法基于正则表达式模式将字符串分割成数组。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // 输出：
        // one
        // two
        // three
        // four
        // five
    }
}
```

### 使用第三方库
虽然.NET框架为正则表达式提供了广泛的支持，但也有像`PCRE.NET`这样的第三方库，它们在C#中提供Perl兼容的正则表达式（PCRE）。如果您需要Perl的正则表达式引擎中未在.NET实现中提供的功能或语法，这会很有用。

要使用`PCRE.NET`，您首先需要安装它的NuGet包，然后您可以类似于使用原生.NET正则表达式类的方式来使用它。

```csharp
// 使用PCRE.NET的示例在此处
// 注意：想象一个类似于上述示例的示例，专门用于展示PCRE.NET独有的功能。
```

在集成第三方正则表达式库时，始终咨询它们的文档，以获取详细的使用和兼容性信息。
