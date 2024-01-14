---
title:                "C#: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将字符串转换为小写。这可能是因为我们需要统一字符串的格式，或者我们需要与其他字符串进行比较。无论是什么原因，掌握如何将字符串转换为小写是非常有用的。

## 如何操作

```C#
string input = "Hello World";
string output = input.ToLower();
Console.WriteLine(output);
```

```C#
string input = "GREETINGS";
string output = input.ToLower();
Console.WriteLine(output);
```

输出：

```
hello world
greetings
```

## 深入了解

字符串转换为小写的过程其实很简单。首先，我们需要获取要转换的字符串，这可以通过用户输入、文件读取等方式来实现。然后，我们使用ToLower()方法来将字符串转换为小写。该方法会返回一个新的字符串，而原始字符串并不会被修改。最后，我们可以将转换后的字符串用于我们的需求。需要注意的是，ToLower()方法只会将大写字母转换为小写，而不会对数字和符号产生影响。

## 参考链接

- [String.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [C# String to Lower (C# Corner)](https://www.c-sharpcorner.com/article/C-Sharp-string-to-lower/)
- [Convert String to Lowercase in C# (GeeksforGeeks)](https://www.geeksforgeeks.org/convert-string-lowercase-c-sharp/)

## 参见

- [String.ToUpper in C# (Mandarin)](https://github.com/jinudaniel/dotnet-converting-strings-lowercase-TO-HIGHERCASE/blob/master/StringToUpper.cs)