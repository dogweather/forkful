---
title:                "查找字符串的长度"
html_title:           "Swift: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为何要找出字符串的长度

在进行字符串操作时，我们经常会需要知道字符串的长度。比如说，当我们需要限制用户输入的密码长度，或者统计文章的字数时，就需要找出字符串的长度。通过这篇文章，你将学习如何使用Swift来找出字符串的长度。

## 如何找出字符串的长度

首先，我们需要定义一个字符串变量，并且使用 `.count` 方法来找出它的长度。例如：

```Swift
var str = "Hello, world!"
print(str.count)
```

以上代码将输出 `13`，因为这个字符串由 13 个字符组成。如果我们将字符串变量赋值为空，那么 `.count` 方法也会返回 `0`，因为空字符串的长度为 0。例如：

```Swift
var emptyStr = ""
print(emptyStr.count)
```

同时，我们也可以使用 `.characters.count` 方法来找出字符串的长度。这两个方法的作用是相同的，但是在以后的版本中 `.characters.count` 方法可能会被移除。因此，建议使用 `.count` 方法来找出字符串的长度，以保证代码的兼容性。例如：

```Swift
var str = "Hello, world!"
print(str.characters.count)
```

## 深入了解字符串的长度

在 Swift 中，字符串的长度并不仅仅指字符的个数，而是 Unicode 中的 Extended Grapheme Cluster 数量。Extended Grapheme Cluster 是多个 Unicode 标量的组合，可以代表一个字符。这导致有些字符的长度看起来是一个，实际上却不是。例如：

```Swift
var str = "👩‍💻"
print(str.count) // 1
```

以上代码返回的长度是 1，但是实际上这个字符串由两个 Extended Grapheme Cluster 组成，每个代表一个字符。如果我们使用 `.unicodeScalars.count` 方法来找出字符串的长度，返回的就是 6，因为每个字符都由多个 Unicode 标量组成。例如：

```Swift
var str = "👩‍💻"
print(str.unicodeScalars.count) // 6
```

最后，需要注意的是，如果我们在字符串中包含了 Emoji 或者其他表情符号，使用 `.count` 方法来找出长度时会返回错误的结果，因为这些表情符号都由多个 Extended Grapheme Cluster 组成。因此，在处理包含表情符号的字符串时，建议使用 `.unicodeScalars.count` 方法来确保得到正确的结果。

## 参考链接

- [Swift 官方文档 - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 官方文档 - Unicode](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID303)
- [Swift-China 官方论坛](https://www.swiftchina.cn/)