---
title:                "将字符串转换为小写"
html_title:           "Swift: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写？

有时候，我们需要对字符串进行比较或者处理时，大小写是一个重要的因素。将字符串统一转换为小写可以避免大小写带来的干扰，简化编程流程。

## 如何进行字符串小写转换

```Swift
// 创建一个字符串变量
var str = "Hello, World!"

// 将字符串转换为小写
let lowerCaseStr = str.lowercased()

// 输出结果为 "hello, world!"
print(lowerCaseStr)
```

## 深入了解字符串小写转换

在 Swift 中，字符串是不可变的，也就是说当我们对字符串进行各种操作后，并不会改变原始字符串本身。因此，当我们对字符串进行小写转换时，会返回一个新的字符串，而不是修改原始字符串。这种不可变性的特性可以保证数据的安全性和稳定性。

## 参考资料

- [Apple Developer Documentation: String.lowercased()](https://developer.apple.com/documentation/swift/string/1786492-lowercased)
- [Swift.org: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hacking with Swift: How to lowercase strings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-lowercase-strings-in-swift)

## 参考链接