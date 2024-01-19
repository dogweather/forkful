---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么&为什么？

字符串链接(Concatenating Strings)是一个将两个或多个字符串组合成一个字符串的过程。程序员这样做是为了更好地组织和展示视觉数据。

## 如何操作：

以下是C#代码示例和其输出：

```C#
string firstName = "Wang";
string lastName = "Li";
string fullName = firstName + " " + lastName; 

Console.WriteLine(fullName);
```
输出:

```C#
Wang Li
```

## 深入探索：

在早期的编程语言中，如C，链接字符串需要进行更复杂的操作。随着C#和其他现代编程语言的发展，这个过程已经变得更加方便和直接。

除了"+"运算符之外，C#还提供了其他几种链接字符串的方式。例如，String.Concat，String.Join和StringBuilder都是一些有效的替代选项。

从底层实现上看，C#通过建立一个新的内存地址来存储被链接的字符串，而不是在原有字符串的内存地址基础上进行修改。这使得字符串链接操作在C#中更加安全。

## 更多资料：

关于字符串连接的更多信息，可以参考Microsft官方文档：[StringBuilder Class](https://docs.microsoft.com/dotnet/api/system.text.stringbuilder). 还可以读一下以下的其他资料：[C# String Manipulation](https://www.c-sharpcorner.com/UploadFile/mahesh/string-manipulation-in-C-Sharp/).