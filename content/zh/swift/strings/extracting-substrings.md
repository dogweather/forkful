---
date: 2024-01-20 17:46:24.098162-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.436601-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Swift\u7684\u65E9\u671F\u7248\u672C\
  \u4E2D\uFF0C\u5904\u7406\u5B57\u7B26\u4E32\u548C\u5B50\u5B57\u7B26\u4E32\u662F\u6BD4\
  \u8F83\u590D\u6742\u7684\u3002\u968F\u7740\u8BED\u8A00\u7684\u6F14\u8FDB\uFF0CApple\u4F7F\
  \u8FD9\u4E9B\u64CD\u4F5C\u53D8\u5F97\u66F4\u52A0\u76F4\u89C2\u548C\u9AD8\u6548\u3002\
  \u5176\u4ED6\u8BED\u8A00\u5982Python\u6709\u4E0D\u540C\u7684\u65B9\u6CD5\u5904\u7406\
  \u5B50\u5B57\u7B26\u4E32\uFF0C\u4F8B\u5982\u4F7F\u7528\u5207\u7247\uFF08slicing\uFF09\
  \u3002Swift\u4E2D\u7684\u5B50\u5B57\u7B26\u4E32\u4E0E\u539F\u59CB\u5B57\u7B26\u4E32\
  \u5171\u4EAB\u5B58\u50A8\u7A7A\u95F4\uFF0C\u56E0\u6B64\u63D0\u53D6\u5B50\u5B57\u7B26\
  \u4E32\u65F6\u4E0D\u4F1A\u8FDB\u884C\u590D\u5236\uFF0C\u8FD9\u6837\u53EF\u4EE5\u63D0\
  \u9AD8\u6027\u80FD\u3002"
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
