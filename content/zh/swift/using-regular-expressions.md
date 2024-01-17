---
title:                "用正则表达式"
html_title:           "Swift: 用正则表达式"
simple_title:         "用正则表达式"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 什么是正则表达式？

正则表达式是一种用于匹配文本模式的工具，它允许程序员通过使用特殊字符和语法来查找和操作文本。程序员通常会使用正则表达式来查找或提取特定格式的数据，例如电话号码、电子邮箱地址或者日期。

为什么程序员要使用正则表达式？

使用正则表达式可以让程序员更加高效地处理和操作文本数据，因为它们不仅可以匹配简单的字母和数字，还可以使用特殊的匹配规则来查找更复杂的文本模式。这样，程序员就可以轻松地从文本数据中提取所需的信息，而不需要手动处理大量的文本。

## 如何使用？

使用正则表达式的第一步是构建一个模式，该模式描述了想要匹配的特定文本格式。例如，如果想要查找所有的邮箱地址，可以使用以下正则表达式模式：```[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}```。然后，可以使用该模式来搜索或提取文本数据，例如：

```Swift 
let text = "我的邮箱地址是hello@example.com"
if let emailRange = text.range(of: "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}", options: .regularExpression) {
    let email = text[emailRange]
    print(email)
}

// 输出： hello@example.com
```

注意：在Swift中，可以使用```range(of:options:)```方法和```regularExpression```选项来搜索或提取文本数据。

## 深入挖掘

- 历史背景：正则表达式最早是由计算机科学家Ken Thompson和Alfred Aho在上世纪60年代提出的，并在Unix操作系统中得到了应用。
- 替代方案：除了使用正则表达式，程序员还可以使用字符串方法、自定义函数或者其他文本匹配工具来处理文本数据。
- 实现细节：正则表达式由特殊的字符和语法组成，每个字符都代表不同的匹配规则。例如，```[A-Z]```表示匹配任意一个大写字母，而```+```表示匹配一个或多个相同的字符。

## 查看更多

- [Swift正则表达式指南](https://www.avanderlee.com/swift/regular-expressions/)
- [正则表达式30分钟入门教程](https://deerchao.cn/tutorials/regex/regex.htm)
- [语法参考](https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_lexical-structure-regular-expression-literal)