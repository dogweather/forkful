---
title:    "Swift: 字符串大写"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转为大写？

很多时候，我们需要处理来自用户输入或外部数据源的字符串。 在这些情况下，可能会出现一些小写或大小写混合的字符串，而我们希望它们保持统一格式。将字符串转换为大写是一种简单有效的方法，能够在处理和比较字符串时确保一致性。

## 如何实现字符串大写化

在Swift中，有几种方法可以将字符串转换为大写。下面是两种常用的方法示例及输出：

```Swift
// 方法一：使用uppercased()方法
let str1 = "hello world"
let caps1 = str1.uppercased()
print(caps1) // 输出：HELLO WORLD

// 方法二：使用uppercaseString变量
let str2 = "apple"
let caps2 = str2.uppercased()
print(caps2) // 输出：APPLE
```

## 深入探讨字符串大写化

除了常用的上述方法外，Swift还提供了一些其他选项来处理字符串的大写化。例如，我们可以选择忽略字符串中的特定字符或特殊符号，以及自定义字符串的大写风格。在实现字符串大写化时，我们还需要注意字符串中可能包含的其他语言字符，以确保正确的转换。

# 参考链接

- [Swift字符串文档](https://developer.apple.com/documentation/swift/string)
- [Swift大写化方法](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID305)
- [Swift中的字符串转换为大写](https://www.swiftbysundell.com/basics/uppercasing-strings/) 
- [字符串大写化的性能优化](https://medium.com/better-programming/a-nice-trick-to-boost-up-your-swift-code-performance-1012998a80d1)