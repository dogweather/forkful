---
title:                "C#: 串联字符串"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么：为什么要参与字符串拼接？

在编程中，我们经常需要将多个字符串组合起来形成一个新的字符串。这种操作称为字符串拼接，它可以帮助我们更有效地创建和处理文本数据。接下来，我们将介绍如何使用 C# 来进行字符串拼接。

## 如何进行字符串拼接

在 C# 中，我们可以使用 "+" 符号来连接两个或多个字符串，如下所示：
```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```
这段代码将输出 "John Doe"，通过将两个字符串和一个空格连接在一起，我们成功合并了两个名字。

除了使用"+"符号，我们还可以使用字符串内插来进行字符串拼接。字符串内插使用"$"符号，允许我们将变量直接插入到字符串中，如下所示：
```C#
string language = "C#";
string intro = $"I love {language} programming.";
Console.WriteLine(intro);
```
这段代码会输出 "I love C# programming."，通过在字符串中插入变量，我们可以更加方便地构建复杂的字符串。

除了以上两种方法，我们还可以使用 String 类的 Concat 方法来进行字符串拼接，该方法可以接收多个字符串作为参数，如下所示：
```C#
string str1 = "Hello";
string str2 = "World";
string str3 = "!";
string greeting = String.Concat(str1, " ", str2, str3);
Console.WriteLine(greeting);
```
这段代码将输出 "Hello World!"，通过将多个字符串按顺序连接起来，我们可以得到一个新的字符串。

## 深入了解字符串拼接

在进行字符串拼接时，我们需要注意一个重要的概念，即字符串是不可变的。这意味着每次对字符串的操作都会创建一个新的字符串对象，所以如果需要频繁地对字符串进行修改，建议使用 StringBuilder 类来提高性能。

另外，除了上面介绍的方法，还有一些高级的字符串拼接技巧，比如使用 String.Format 方法和使用字符串插值来创建格式化字符串。在实际开发中，我们可以根据场景选择合适的方法来进行字符串拼接。

## 参考链接

- [C# 字符串拼接（Concat、+、StringBuilder、String.Format、字符串内插法）](https://www.cnblogs.com/wangminjin/p/9098067.html)
- [C# 教程 - 字符串拼接](https://www.runoob.com/csharp/csharp-string-concat.html)
- [MSDN - String Interpolation (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [MSDN - String.Concat Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=netframework-4.8)
- [MSDN - String.Format Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netframework-4.8)
- [MSDN - StringBuilder Class (System.Text)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netframework-4.8)

## 查看更多

希望本文能帮助您更好地理解和使用 C# 中的字符串拼接。如果您对 C# 还有其他疑问，建议您查看上述参考链接或者搜索更多相关资料。