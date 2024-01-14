---
title:                "Swift: 将一个字符串转换为小写"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换成小写
在Swift编程中，经常会遇到要将字符串转换成小写的需求。这个简短的过程可能会让您有所困惑，但实际上它有着重要的作用。下面我们来看看如何实现这个转换，以及更深入的探讨它的原理。

## 如何实现
下面是一个简单的Swift代码段，展示了如何将一个字符串转换成小写，并输出结果。

```Swift
let originalString = "Hello, Swift Programming"
let lowercaseString = originalString.lowercased()
print(lowercaseString)
```

输出结果为：

```Swift
hello, swift programming
```

## 深入探讨
在Swift中，有两种方法可以将字符串转换成小写。第一种方法是使用`lowercased()`函数，这个函数将会返回一个转换后的字符串。另一种方法是使用`map`函数，在这种方法中，我们将字符串的每一个字符都转换成小写，然后再将它们重新组合成一个新的字符串。

除了大小写转换，还有一些其他的转换方法可以帮助您更有效地处理字符串。例如，`uppercased()`函数可以将字符串转换成大写，`capitalized()`函数可以将字符串首字母大写。

无论您使用哪种方法，都应该注意到这些字符串转换并不会改变原始字符串本身，而是返回一个新的转换后的字符串。这种操作是非常安全的，因为它不会改变原始数据，只是在需要的时候返回一个处理后的副本。

# 查看更多
- [Swift字符串文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift常用字符串操作技巧](https://medium.com/swift-programming/swift-common-string-manipulations-c69e0cda5b80)
- [Swift字符串转换方法比较](https://www.hackingwithswift.com/articles/151/how-to-capitalize-the-first-letter-of-a-string)

# 查看更多