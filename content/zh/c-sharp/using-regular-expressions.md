---
title:                "使用正则表达式"
html_title:           "C#: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 在C#中使用正则表达式：一个简洁的指南

## 什么和为什么？

正则表达式是一种用于处理文本的技术，它可以帮助程序员快速且有效地识别和匹配特定的文本模式。程序员通常会使用正则表达式来处理一些常见的任务，比如验证用户输入的数据、从大量文本中提取信息等。

## 如何：

下面是一个简单的例子，演示如何使用正则表达式来匹配一个邮件地址并提取出其中的用户名和域名：

```C#
string email = "john.doe@example.com";

// 使用正则表达式匹配用户名和域名
Match match = Regex.Match(email, @"^([\w\.\-]+)@([\w\-]+)((\.(com|org|net|gov|edu))(\.[a-z]{2,})?)$");

// 输出提取到的用户名和域名
Console.WriteLine("用户名： " + match.Groups[1].Value);
Console.WriteLine("域名： " + match.Groups[2].Value);
```

输出结果为：

```
用户名： john.doe
域名： example
```

## 深入了解：

### 历史背景：

正则表达式最早由计算机科学家Stephen Kleene于1950年提出，它是一种被广泛使用的文本处理工具，被集成在许多编程语言中，包括C#。

### 其他选择：

除了正则表达式，程序员也可以使用字符串处理函数，如Substring和Contains来处理文本。然而，正则表达式通常更灵活和强大，能够提供更精确的匹配。

### 实现细节：

在C#中，使用System.Text.RegularExpressions命名空间中的类来实现正则表达式功能。Match类表示一个成功匹配的结果，Regex类用于构建正则表达式。

## 查看更多：

如果你想深入学习正则表达式，你可以查看以下资源：

- [Microsoft官方文档-正则表达式](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [C#简易入门教程-正则表达式](https://www.c-sharpcorner.com/article/learn-regular-expression-in-c-sharp/)
- [Regex101 - 在线正则表达式测试工具](https://regex101.com/)