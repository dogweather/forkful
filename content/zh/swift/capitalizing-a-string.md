---
title:                "将字符串转化为大写"
html_title:           "Swift: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

字符串大写化是指把一个字符串中所有的字符转换为大写。编程人员这么做通常是为了实现格式标准化和更好的显示效果。

## 如何实现

在 Swift 中，我们可以使用 `uppercased()` 方法来实现字符的大写化。以下是代码和输出实例：

```Swift
let myString = "hello, world!"
let uppercasedString = myString.uppercased()
print(uppercasedString)  // 输出："HELLO, WORLD!"
```

## 深层次解析

1. **历史背景**：字符串大写化在编程中有长久的历史，其实现方式在不同的编程语言中略有不同。在 Swift 中，`uppercased()` 方法可以直接实现该功能。

2. **替代方法**：另一个类似的方法是 `capitalized`，它会将字符串中的每个词的首字母转化为大写。

3. **实现细节**：`uppercased()` 方法的实现取决于 Unicode 标准。它遵循 Unicode 属性的规范，将字符串中的所有字符转化为其对应的大写形式。

## 参考链接

1. [Swift 语言字符串和字符](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Unicode 标准](http://www.unicode.org/reports/tr44/)
3. [Swift `String` API 参考](https://developer.apple.com/documentation/swift/string)