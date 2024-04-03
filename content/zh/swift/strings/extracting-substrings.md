---
date: 2024-01-20 17:46:24.098162-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF0C\u5C31\u662F\u4ECE\u4E00\u4E2A\
  \u8F83\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5B57\u7B26\u6216\
  \u8005\u4E00\u6BB5\u7279\u5B9A\u7684\u5B57\u7B26\u5E8F\u5217\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u5904\u7406\u3001\u5206\
  \u6790\u6216\u8005\u662F\u7B80\u5316\u641C\u7D22\u548C\u6392\u5E8F\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.145342-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF0C\u5C31\u662F\u4ECE\u4E00\u4E2A\
  \u8F83\u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5B57\u7B26\u6216\
  \u8005\u4E00\u6BB5\u7279\u5B9A\u7684\u5B57\u7B26\u5E8F\u5217\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u5904\u7406\u3001\u5206\
  \u6790\u6216\u8005\u662F\u7B80\u5316\u641C\u7D22\u548C\u6392\u5E8F\u64CD\u4F5C\u3002\
  ."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
