---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
正则表达式用于模式匹配和文本处理，非常灵活。程序员用它们来快速找到、替换或检查复杂的文本模式，节省时间和精力。

## How to: 如何操作？
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string text = "您好，我的邮箱是 example@example.com";
        string pattern = @"\w+@\w+\.\w+";

        Match match = Regex.Match(text, pattern);
        if (match.Success)
        {
            Console.WriteLine($"找到邮箱: {match.Value}");
        }
    }
}
```
输出:
```
找到邮箱: example@example.com
```

## Deep Dive 深入探讨
正则表达式起源于上世纪50年代的理论计算机科学。而在.NET框架中，正则表达式由 `System.Text.RegularExpressions` 命名空间提供支持。除了内置方法外，你还可以使用LINQ或者字符串的内置方法进行文本处理，但正则表达式提供更强大的模式匹配功能。性能方面，恰当使用时正则表达式可以很快，但复杂的表达式可能会慢。

## See Also 参考链接
- [正则表达式 - .NET](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [在线正则表达式测试器](https://regex101.com/)
- [正则表达式学习资源 - RegexOne](https://regexone.com/)
