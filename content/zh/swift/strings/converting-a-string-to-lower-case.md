---
date: 2024-01-20 17:39:27.227522-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Swift\u4E2D\uFF0C\u5B57\
  \u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u975E\u5E38\u7B80\u5355\u3002\u4F7F\u7528\
  `lowercased()`\u65B9\u6CD5\u5373\u53EF\u3002\u5982\u4E0B\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.434599-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Swift\u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u8F6C\u6362\u4E3A\u5C0F\u5199\u975E\u5E38\u7B80\u5355\u3002\u4F7F\u7528`lowercased()`\u65B9\
  \u6CD5\u5373\u53EF\u3002\u5982\u4E0B\u4F8B\uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
