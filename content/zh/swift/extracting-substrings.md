---
title:                "Swift: 从计算机编程到提取子串"
simple_title:         "从计算机编程到提取子串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取字符串是编程中常见的任务，它允许我们从一个长的字符串中提取出我们需要的部分。这个过程被称为“提取子字符串”，在Swift中，我们可以使用一些方法来实现这一点。

## 怎么做

让我们假设我们有一个包含电子邮件地址的长字符串。我们想要从中提取出用户名部分。下面是一个简单的实现过程：

```Swift 
let str = "johndoe@example.com"
let username = str[str.startIndex...str.firstIndex(of: "@")!]
print(username) // "johndoe"
```

我们首先使用字符串的`startIndex`方法来获取整个字符串的开头位置。然后，通过`firstIndex(of: "@")`方法来获取`@`符号的位置，并将其作为结束位置。最后，我们通过使用`subscript`来截取出我们需要的子字符串。

还有其他方法可以实现相同的功能，比如使用`prefix`和`suffix`方法来提取出前几个字符或后几个字符。

```Swift
let str = "Hello World"
let prefix = str.prefix(5) // "Hello"
let suffix = str.suffix(5) // "World"
```

除了上面提到的方法，我们还可以使用`range`来指定一个范围来提取子字符串。

```Swift
let str = "Welcome to Swift"
let range = str.index(str.startIndex, offsetBy: 11)..<str.endIndex
let result = str[range] // "Swift"
```

## 深入了解

除了上面提到的方法，我们还可以通过使用`unicodeScalars`和`utf8`来处理字符串中的Unicode字符和UTF-8编码。另外，我们还可以使用正则表达式来提取符合特定模式的子字符串。

## 参考资料

- [Swift官方文档 - 字符串和字符](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift字符串教程](https://www.tutorialspoint.com/swift/swift_strings.htm)
- [使用Swift解析字符串](https://www.hackingwithswift.com/articles/214/how-to-parse-a-string-using-swift)