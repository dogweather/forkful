---
title:                "C#: 提取子字符串"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么会提取子字符串？

提取子字符串是一种很常用的编程技巧，可以帮助我们从一个大字符串中获取特定的信息或者处理特定的数据。例如，在处理用户输入或者读取文件时，我们可能需要从字符串中提取出特定的信息，这时就可以用到提取子字符串的方法。

## 如何提取子字符串？

```C#
string sentence = "今天是周六，明天是周天，周一就要上班了";
string substring = sentence.Substring(2, 4);
// Output: 不是周
```

上面的代码展示了如何用 C# 的 `Substring` 方法来提取一个字符串的子字符串。首先，我们定义了一个字符串 `sentence`，它包含了一些日期信息。然后，我们使用 `Substring` 方法来从 `sentence` 中提取出从第2个字符开始的长度为4的子字符串。最后的输出结果就是 `"不是周"`，这正是我们所期望的结果。

除了 `Substring` 方法之外，我们还可以使用 `Split` 方法来根据特定的分隔符将一个字符串分割成多个子字符串。例如，假设我们有一个包含多个名字的字符串，每个名字之间用逗号隔开，我们就可以用 `Split` 方法来提取出每个名字，并且将它们存放到一个字符串数组中。

```C#
string names = "李白, 杜甫, 王安石, 苏轼";
string[] nameList = names.Split(",");
// Output: ["李白", "杜甫", "王安石", "苏轼"]
```

## 深入了解提取子字符串

提取子字符串的方法可以帮助我们在处理字符串时更加方便和高效，但是在实际使用时还需要注意一些细节。例如，用 `Substring` 方法提取子字符串时，需要指定起始位置和长度，如果我们提供的长度超过了字符串的实际长度，就会产生错误。此外，在使用 `Split` 方法时，也需要注意分隔符的选择，以及可能出现的空格或者其他特殊字符。

另外，对于一些复杂的字符串处理需求，可能需要结合正则表达式来提取子字符串，这也是一个值得深入学习的技巧。

## 参考文献

- [C# 中的字符串处理方法](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [使用正则表达式在 C# 中提取子字符串](https://www.dotnetperls.com/regex)
 
## 参见

- [Visual Studio 官方文档 - Strings (C# Programming Guide)](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/strings)
- [W3School - C# Substring() Method](https://www.w3schools.com/cs/tryit.asp?filename=demo_ref_string_substring)
- [Basic C# Program Examples - Split Strings](https://www.basic-csharp-programming.com/csharp-split-strings.html)