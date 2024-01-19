---
title:                "将字符串大写"
html_title:           "PowerShell: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
把字符串中的每个词的首字母转换为大写，就是大写字符串。程序员通常会这样操作，原因为实现编码的标准化和格式化。

## 如何操作：
以下是如何在 PowerShell 中把字符串大写的代码示例以及运行结果：

```PowerShell
$str = "hello world"
$str = (Get-Culture).TextInfo.ToTitleCase($str.ToLower())
$str
```

运行结果：

```PowerShell
Hello World
```

在上述代码中，我们首先定义一个小写的字符串 `$str`。然后，我们使用 `(Get-Culture).TextInfo.ToTitleCase()` 函数将字符串 `$str` 中的每个单词的首字母大写。最后，我们输出大写后的字符串。

## 深入探讨：
字符串大写的概念在计算机编程历史中扮演着重要的角色。在早期，存储和处理能力有限，人们使用大写字母来节省空间。现在，尽管我们不再受这些限制，但是大写还是在很多场合被用到，包括搜索、用户输入校验以及创建易于阅读的输出。

在 PowerShell 中，除了使用 `(Get-Culture).TextInfo.ToTitleCase()` 方法，我们还可以在 .NET 中调用 `System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase()` 来实现大写功能。但是，它们的工作原理基本相同。

## 参考信息：
对于想要深入理解字符串大写以及 PowerShell 的读者，以下是一些额外的参考链接：
1. [PowerShell 文档](https://docs.microsoft.com/en-us/powershell/)
2. [.NET Culture 文档](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
3. [字符串大小写定义和历史](https://en.wikipedia.org/wiki/Letter_case)