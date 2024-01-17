---
title:                "提取子字符串"
html_title:           "Swift: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是子字符串？为什么程序员要这么做？
子字符串就是从一个字符串中提取出一部分字符组成的新字符串。程序员经常这么做是为了对文本数据进行处理，例如搜索、替换或者格式化。

## 如何实现？
使用Swift的```substring```方法可以轻松地从一个字符串中提取子字符串。例如，我们有一个字符串 ```let phrase = "我爱编程"```，若要提取出其中的 ```"编程"``` 这个词，只需要使用 ```let substring = phrase.substring(from: 2)``` 。

这会把字符串中索引为2（即第三个字符）及之后的部分提取出来。输出为 ```"编程"``` 。

另一个有用的方法是使用下标语法 ```[ ]``` 将字符串与指定的索引位置结合起来。例如，若要提取字符串中索引为2到4的这一部分字符，在```let substring = phrase[2...4]```中，使用省略号表示范围。这会把字符串中索引为2至4的字符提取出来，输出为 ```"编程"```。

## 深入了解
子字符串的概念可以追溯到C语言中的```substring```函数，它的操作与Swift中的substring方法类似。除了使用substring方法外，也可以使用NSString的substring方法来从Swift字符串中提取子字符串。

此外，还有一些替代方法，例如使用NSRegularExpression来提取符合特定模式的子字符串。

实现子字符串的原理是利用字符串的索引和范围来获取指定位置的字符或字符序列，并将它们组合成新的字符串。由于Swift中的字符串类型是值类型，因此在提取子字符串时，会产生一个新的字符串实例。

## 参考资料
[Swift指南: 字符串和字符](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

[Swift官方文档: Substrings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID319)

[String类文档: substring](https://developer.apple.com/documentation/swift/string/1688451-substring)

[String类文档: substring(from:)](https://developer.apple.com/documentation/swift/string/1540984-substring)