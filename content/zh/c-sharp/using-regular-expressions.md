---
title:                "C#: 使用正则表达式"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 为什么要使用正则表达式

在编写C#程序时，您可能会遇到需要对文本进行复杂匹配和替换的情况。这时候，正则表达式就是您的好帮手。使用正则表达式，您可以通过一系列规则来查找和修改文本，极大地提高了文本处理的效率。

## 如何使用正则表达式

使用C#内置的regex类，您可以轻松创建和管理正则表达式。下面是一个简单的示例代码，演示如何使用正则表达式来匹配和替换文本中的数字。

```C#
string text = "我的电话号码是12345678。";
string pattern = @"\d+";
string replacement = "[电话号码]";
string result = Regex.Replace(text, pattern, replacement);
Console.WriteLine(result);
```

输出结果将会是`我的电话号码是[电话号码]。`，其中的数字被替换成了指定的文字。

## 深入了解正则表达式

正则表达式是一门强大的文本处理工具，不仅仅可以用来简单的匹配和替换。通过学习更多的语法和特殊符号，您可以轻松地实现更复杂的文本操作。例如，使用正则表达式可以轻松地从一个HTML文档中抽取所有的链接地址。

## 参考链接

- C# 正则表达式教程（https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/regular-expression-language-quick-reference）
- 正则表达式在线练习平台（https://regexr.com/）
- 在C#程序中使用正则表达式的最佳实践（https://www.c-sharpcorner.com/article/best-practices-to-use-regular-expressions-in-C-Sharp/）

# 参见

- [C# String 类的文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.string?view=netcore-3.1)
- [C#正则表达式的高级用法](https://www.c-sharpcorner.com/UploadFile/89b85a/higher-usage-of-regular-expression-in-C-Sharp/)