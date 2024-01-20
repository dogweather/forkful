---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么 & 为什么? 

删除字符是指从特定字符串中移除匹配某个规则的一部分或全部字符。这是为了清洁数据，保持字符串的整洁，符合处理要求。

## 如何操作:
在 C# 中，我们通常使用`Regex.Replace()`方法和正则表达式来删除匹配到的字符。查看以下示例:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string pattern = "[0-9]"; // Matches any single digit
        string input = "Hello123World456";
        string replacedString = Regex.Replace(input, pattern, "");

        Console.WriteLine($"Before: {input}");
        Console.WriteLine($"After: {replacedString}");
    }
}
```

运行这段代码，输出结果将会是:
```
Before: Hello123World456
After: HelloWorld
```

## 深入了解

1. 历史背景: `Regex.Replace()`方法是在.NET Framework的初期版本中引入的，使得在C#中使用正则表达式操作字符串变得可行且强大。
2. 替代方案: 如果只是想删除特定的字符，而不是模式，可以使用`String.Replace()`方法
3. 实现细节： `Regex.Replace()` 方法工作原理是在输入字符串中查找匹配正则表达式模式的所有子字符串，并用第三个参数指定的字符串替换这些子字符串。

## 参考链接

1. Microsoft Docs: [正则表达式语言 - 快速参考](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference)
2. Microsoft Docs: [Regex.Replace方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
3. Stack Overflow: [如何在C#中使用正则表达式删除字符串中的特定字符](https://stackoverflow.com/questions/1178814/how-to-remove-any-specific-characters-in-a-string-using-regex-in-c-sharp)