---
aliases:
- /zh/swift/converting-a-string-to-lower-case/
date: 2024-01-20 17:39:27.227522-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u662F\u628A\u6240\
  \u6709\u6587\u672C\u5B57\u7B26\u8F6C\u6362\u6210\u5C0F\u5199\u5F62\u5F0F\u7684\u8FC7\
  \u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u7EDF\
  \u4E00\u6570\u636E\u683C\u5F0F\uFF0C\u4FBF\u4E8E\u6BD4\u8F83\u548C\u641C\u7D22\uFF0C\
  \u7279\u522B\u662F\u5728\u4E0D\u533A\u5206\u5927\u5C0F\u5199\u7684\u60C5\u51B5\u4E0B\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.430976
model: gpt-4-1106-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u662F\u628A\u6240\
  \u6709\u6587\u672C\u5B57\u7B26\u8F6C\u6362\u6210\u5C0F\u5199\u5F62\u5F0F\u7684\u8FC7\
  \u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u7EDF\
  \u4E00\u6570\u636E\u683C\u5F0F\uFF0C\u4FBF\u4E8E\u6BD4\u8F83\u548C\u641C\u7D22\uFF0C\
  \u7279\u522B\u662F\u5728\u4E0D\u533A\u5206\u5927\u5C0F\u5199\u7684\u60C5\u51B5\u4E0B\
  \u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

将字符串转换为小写是把所有文本字符转换成小写形式的过程。程序员这样做主要是为了统一数据格式，便于比较和搜索，特别是在不区分大小写的情况下。

## How to: (如何操作：)

在Swift中，字符串转换为小写非常简单。使用`lowercased()`方法即可。如下例：

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)
```

输出将是：

```
hello, world!
```

## Deep Dive (深入了解)

在Swift早期版本中，字符串操作已经非常强大。`lowercased()`是`String`类型的一个方法，它遵循Unicode标准来转换大小写。它不仅适用于英文字符，而且适用于许多其他语言的字符。

替代方案不多，因为Swift的`lowercased()`已经非常高效和全面。然而，在某些复杂的语言规则中，可能需要专业的本地化支持来处理特殊情况。

在底层实现上，`lowercased()`方法通常涉及到Unicode标量的查询和替换。它将检查每个字符的Unicode表示，并寻找其对应的小写版本，如果存在的话。

## See Also (另请参阅)

- Swift标准库文档中的`String`：[https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- Unicode大小写映射信息：[http://www.unicode.org/Public/UNIDATA/CaseFolding.txt](http://www.unicode.org/Public/UNIDATA/CaseFolding.txt)
- Swift字符串和字符指南：[https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
