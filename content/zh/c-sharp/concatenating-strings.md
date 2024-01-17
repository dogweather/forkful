---
title:                "串连字符串"
html_title:           "C#: 串连字符串"
simple_title:         "串连字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

C#: 编程中连接字符串

在C＃中，连接字符串是将两个或多个字符串合并成一个字符串的过程。这通常用于创建更大的字符串，如日志消息、错误消息等等。程序员通常会使用连接字符串来动态生成字符串，以便满足不同场景的需求。

## 怎么使用

要连接字符串，我们可以使用“+”操作符或使用`String.Concat`方法。下面是使用这两种方法的示例代码和输出：

```
string s1 = "Welcome";
string s2 = "to";
string s3 = "C#";

// 使用“+”操作符
string result1 = s1 + " " + s2 + " " + s3;
Console.WriteLine(result1); // 输出: Welcome to C#

// 使用String.Concat方法
string result2 = String.Concat(s1, " ", s2, " ", s3);
Console.WriteLine(result2); // 输出: Welcome to C#
```

在以上代码中，我们首先创建了三个字符串变量，分别存储"Welcome"、"to"和"C#"。然后，我们使用两种不同的方法来连接这些字符串，并打印出结果。

## 深入了解

连接字符串的历史可以追溯到1990年，当时它是C语言的一个标准函数。在C＃中，它是由`String.Concat`和`String.Join`等方法实现的。`String.Join`方法允许我们使用一个分隔符来连接多个字符串，并返回一个单一的字符串。

另外，除了使用"+"操作符和`String.Concat`，我们也可以使用`StringBuilder`类来连接字符串。这个类提供了更高效的方法来处理大量的字符串连接操作。然而，当需要处理少量的字符串连接时，使用"+"操作符和`String.Concat`更简洁易懂。

## 查看更多

- [C# 字符串连接](https://www.runoob.com/csharp/csharp-string.html)
- [C# String.Concat 方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.concat)