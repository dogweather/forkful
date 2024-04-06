---
date: 2024-01-20 17:57:32.874213-07:00
description: "How to: \u600E\u4E48\u505A \u641C\u7D22\u548C\u66FF\u6362\u6280\u672F\
  \u5DF2\u7ECF\u5B58\u5728\u5F88\u4E45\u4E86\uFF0C\u662F\u6587\u672C\u5904\u7406\u7684\
  \u57FA\u7840\u3002\u5728C#\u4E2D\uFF0C`.Replace()` \u65B9\u6CD5\u7B80\u5355\u9AD8\
  \u6548\uFF0C\u80CC\u540E\u5219\u5229\u7528\u4E86\u5B57\u7B26\u4E32\u5904\u7406\u7684\
  \u7B97\u6CD5\u3002\u9664\u6B64\u4E4B\u5916\uFF0C\u8FD8\u53EF\u4EE5\u4F7F\u7528\u6B63\
  \u5219\u8868\u8FBE\u5F0F`Regex`\uFF0C\u63D0\u4F9B\u66F4\u590D\u6742\u7684\u641C\u7D22\
  \u66FF\u6362\u529F\u80FD\uFF0C\u6BD4\u5982\u6A21\u5F0F\u5339\u914D\u548C\u591A\u4E2A\
  \u66FF\u6362\u3002\u5B9E\u73B0\u4E0A\uFF0C`.Replace()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.066579-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u641C\u7D22\u548C\u66FF\u6362\u6280\u672F\u5DF2\u7ECF\
  \u5B58\u5728\u5F88\u4E45\u4E86\uFF0C\u662F\u6587\u672C\u5904\u7406\u7684\u57FA\u7840\
  \u3002\u5728C#\u4E2D\uFF0C`.Replace()` \u65B9\u6CD5\u7B80\u5355\u9AD8\u6548\uFF0C\
  \u80CC\u540E\u5219\u5229\u7528\u4E86\u5B57\u7B26\u4E32\u5904\u7406\u7684\u7B97\u6CD5\
  \u3002\u9664\u6B64\u4E4B\u5916\uFF0C\u8FD8\u53EF\u4EE5\u4F7F\u7528\u6B63\u5219\u8868\
  \u8FBE\u5F0F`Regex`\uFF0C\u63D0\u4F9B\u66F4\u590D\u6742\u7684\u641C\u7D22\u66FF\u6362\
  \u529F\u80FD\uFF0C\u6BD4\u5982\u6A21\u5F0F\u5339\u914D\u548C\u591A\u4E2A\u66FF\u6362\
  \u3002\u5B9E\u73B0\u4E0A\uFF0C`.Replace()` \u4E3B\u8981\u662F\u901A\u8FC7\u904D\u5386\
  \u539F\u5B57\u7B26\u4E32\u5E76\u9010\u4E2A\u66FF\u6362\u6765\u5B8C\u6210\u5DE5\u4F5C\
  \u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: 怎么做
```C#
using System;

class Program
{
    static void Main()
    {
        string originalText = "Hello, World! Programming in C# is fun.";
        string searchText = "World";
        string replaceText = "Universe";
        
        string newText = originalText.Replace(searchText, replaceText);
        
        Console.WriteLine(newText); // 输出: Hello, Universe! Programming in C# is fun.
    }
}
```

## Deep Dive 深入探讨
搜索和替换技术已经存在很久了，是文本处理的基础。在C#中，`.Replace()` 方法简单高效，背后则利用了字符串处理的算法。除此之外，还可以使用正则表达式`Regex`，提供更复杂的搜索替换功能，比如模式匹配和多个替换。实现上，`.Replace()` 主要是通过遍历原字符串并逐个替换来完成工作。

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalText = "Cats & dogs. Dogs and cats.";
        string pattern = @"\b([Cc]ats)\b";
        
        string newText = Regex.Replace(originalText, pattern, "Animals");
        
        Console.WriteLine(newText);  // 输出: Animals & dogs. Dogs and Animals.
    }
}
```

## See Also 另请参阅
- [.NET 文档上的 `String.Replace` 方法](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [.NET 文档上的 `Regex` 类](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- [关于C#编程的在线教程](https://www.learncs.org/)
