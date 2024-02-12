---
title:                "搜索和替换文本"
aliases: - /zh/c-sharp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:32.874213-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
搜索和替换文本就是在字符串中找特定文字，然后用其他文字来替代。程序员这么做是为了批量修改数据，简化代码更新，或是处理用户输入。

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
