---
title:                "提取子字符串"
html_title:           "C#: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

首先，提取子串是一种非常常见的字符串操作，它可以帮助我们从一个字符串中提取出我们想要的部分，比如根据特定的条件筛选出需要的数据。其次，提取子串还可以帮助我们简化字符串处理的过程，提高代码的可读性和清晰度。

## 如何

提取子串在C#中非常简单，我们可以使用 `Substring()` 方法来实现。该方法接受两个参数，第一个参数为要提取子串的起始索引，第二个参数为要提取的字符数量。

下面是一个示例：

```C#
string str = "Hello World";
string subStr = str.Substring(6, 5); // subStr 的值为 "World"
Console.WriteLine(subStr);
```

输出：

```
World
```

## 深入了解

除了使用 `Substring()` 方法，C#还提供了其他一些方法来帮助我们提取子串。例如，`Remove()` 方法可以帮助我们删除字符串中的一部分内容，并返回删除后的结果字符串。`Split()` 方法可以根据指定的分隔符将字符串拆分成多个子串，并返回一个字符串数组。

另外，C#中还提供了一些内置的字符串处理工具类，比如 `Regex` 类可以帮助我们使用正则表达式来提取满足特定条件的子串，`StringBuilder` 类可以帮助我们高效地拼接和处理字符串。

总的来说，提取子串是一项非常简单、常用的字符串操作，但是在实际的开发中也有一些需要注意的地方，比如索引的范围、边界条件等。

## 参考链接

- [MSDN: String.Substring Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [MSDN: String.Remove Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.remove)
- [MSDN: String.Split Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.split)
- [MSDN: System.Text.RegularExpressions.Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- [MSDN: System.Text.StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)