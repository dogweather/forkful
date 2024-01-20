---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？

字符串的长度是指字符串中的字符数。程序员需要测量它，因为在处理文本或解析用户输入时，这对于理解和控制数据非常必要。

## 如何做：

在C#中，可以通过String类的 `Length` 属性来获取字符串的长度。让我们来看一下如何使用它：

```C#
string content = "欢迎来到精彩的C#世界!";
Console.WriteLine("字符串长度 = " + content.Length);
```
输出结果将会是：

```C#
字符串长度 = 11
```
注意:汉字与字母数都计为1。

## 深入探讨

早期的编程语言，例如C，没有任何内置的方式来找出字符串的长度。程序员需要手动计算，这非常耗时。

C# 提供了 `Length` 和 `Count()` 方法来查找字符串长度，其实它们的工作原理是相同的。`Count()` 方法是 System.Linq 命名空间中的扩展方法，它更适用于集合操作。而 `Length` 属性则是字符串类型自带的方法。

获取字符串长度的操作复杂度为O(1)，这意味着无论字符串的长度是多少，执行时间都是常数。原因在于 `Length` 属性并不实际计算字符，而是返回内部已经存储的长度值。

## 另请参阅

关于C#字符串更多细节，你可以参阅下列链接：

1. [C# 字符串（Microsoft官方文档）](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/strings/)
2. [C# 如何测量字符串长度（StackOverflow讨论）](https://stackoverflow.com/questions/2252982/how-to-get-the-length-of-a-string-in-c)
3. [C# LINQ 命名空间（Microsoft官方文档）](https://docs.microsoft.com/zh-cn/dotnet/api/system.linq?view=net-5.0)