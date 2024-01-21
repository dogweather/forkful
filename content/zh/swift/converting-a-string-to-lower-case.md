---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:39:27.227522-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
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