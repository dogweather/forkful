---
title:                "Swift: 将字符串大写化"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

字符串是Swift编程语言中非常重要的一部分。它们可以存储文本，数字和其他数据，并可以在程序中进行操作。有时候，我们需要将字符串转换为大写，这可能是因为用户输入的文本需要统一格式，或者我们需要在输出中使用大写字母。无论是什么原因，字符串的大写转换是一个很常见的任务，在接下来的教程中，我们将学习如何在Swift中轻松完成这个任务。

## 如何

首先，让我们定义一个简单的字符串变量，作为我们操作的例子。

```Swift
let myString = "hello world"
```

如果我们想要将这个字符串转换为全部大写，我们可以使用Swift中的`uppercased()`方法。只需要在我们的变量名后面加上`.`，然后输入方法名，就可以将字符串转换为大写形式。

```Swift
let myUppercasedString = myString.uppercased()
print(myUppercasedString)
```

输出结果为：HELLO WORLD

如果我们想要将第一个字符转换为大写，我们可以使用`capitalized`方法。这个方法只会将第一个字符转换为大写，其他字符仍保持原样。

```Swift
let myCapitalizedString = myString.capitalized
print(myCapitalizedString)
```

输出结果为：Hello world

## 深入学习

除了使用内置的`uppercased()`和`capitalized`方法，我们还可以使用Swift中的其他方法来实现字符串的大写转换，例如使用`==`比较运算符来比较两个字符串是否相等，以及使用`append()`方法来在字符串后面添加字符。

另外，还可以使用string interpolation来在字符串中插入变量，从而实现更加灵活的字符串操作。

## 请参阅

1. [Swift字符串官方文档](https://developer.apple.com/documentation/swift/string)
2. [Swift字符串方法概述](https://www.hackerrank.com/domains/tutorials/30-days-of-code?filters%5Bsubdomains%5D%5B%5D=30-days-of-code&badge_type=30-days-of-code)
3. [Swift字符串操作指南](https://www.tutorialsleeptop.com/swift-string-operations-example/)