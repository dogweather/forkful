---
title:                "将字符串转换为小写"
html_title:           "C#: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么和为什么？

在C＃编程中，将字符串转换为小写是一种常见的操作。这意味着将字符串中的所有字母转换为小写形式。程序员经常这样做是因为在比较字符串时，它可以帮助我们忽略字母的大小写，从而使比较更加准确和精确。

## 如何做？

```
string input = "HeLLo WoRlD";
string output = input.ToLower();
Console.WriteLine(output);
```

这段代码的输出将是 "hello world"。

## 深入探讨
尽管转换字符串到小写看起来很简单，但实际上它涉及到了几个步骤。首先，计算机使用ASCII码来表示字母和符号，每个字母都有一个特定的数字代码。小写字母和大写字母的ASCII码是不同的。因此，当我们将字符串转换为小写时，计算机必须将每个字母的ASCII码转换为对应的小写字母的ASCII码。这就是为什么我们可以使用ToLower（）方法来帮助我们实现这一点。

另一种方法是使用LINQ查询来转换字符串。它可以写成类似下面的形式：

`string output = string.Join("", input.Select(c => char.ToLower(c)));`

这个方法会更慢一些，但是在某些情况下可能更方便和灵活。

## 参考链接

- [String.ToLower 方法 (System) - Microsoft文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.tolower?view=netcore-3.1)
- [ASCII码表 - Baidu百科](https://baike.baidu.com/item/ASCII/309296)