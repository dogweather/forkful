---
title:                "提取子字符串"
aliases:
- zh/swift/extracting-substrings.md
date:                  2024-01-20T17:46:24.098162-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

提取子字符串，就是从一个较长的字符串中获取部分字符或者一段特定的字符序列。程序员这么做通常是为了数据处理、分析或者是简化搜索和排序操作。

## How to: (如何操作：)

```Swift
let phrase = "Hello, Swift developers!"
let startIndex = phrase.index(phrase.startIndex, offsetBy: 7)
let endIndex = phrase.index(phrase.startIndex, offsetBy: 12)
let substring = phrase[startIndex...endIndex]

print(substring) // 输出："Swift"
```

## Deep Dive (深入探讨)

在Swift的早期版本中，处理字符串和子字符串是比较复杂的。随着语言的演进，Apple使这些操作变得更加直观和高效。其他语言如Python有不同的方法处理子字符串，例如使用切片（slicing）。Swift中的子字符串与原始字符串共享存储空间，因此提取子字符串时不会进行复制，这样可以提高性能。

## See Also (另见)

- Swift官方文档关于字符串和字符：[Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Ray Wenderlich的Swift字符串教程：[Swift String Tutorial](https://www.raywenderlich.com/5539282-swift-string-tutorial-frequently-used-operations)
- Swift字符串API参考：[String — Swift Standard Library](https://developer.apple.com/documentation/swift/string)
