---
title:                "Swift: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要把字符串转换为小写？

当我们处理字符串时，有时候需要将其转换为全小写。这可以让字符串在比较和搜索时更容易匹配，也能统一字符大小写，避免出现混乱。

## 如何实现？

```Swift
let string = "HELLO WORLD"
let lowercasedString = string.lowercased()

print(lowercasedString) // "hello world"
```

在以上示例中，我们使用了Swift内置的lowercased()方法，它可以将字符串转换为小写。这个方法会返回一个新的字符串，原始字符串不会被修改。

## 深入探讨

在Swift中，字符串是一个值类型，所以它的值是不可变的。这意味着我们无法直接修改字符串的大小写，而是需要使用一个新的字符串来存储转换后的值。

此外，lowercased()方法只能转换非扩展字符集中的字符，也就是说只能转换英文字母。如果要处理非英文字符，我们可以使用Foundation框架中的NSString对象来实现转换。

## 参考资料

[How to capitalize first letter of a string in Swift?](https://stackoverflow.com/questions/26306326/how-to-capitalize-first-letter-of-a-string-in-swift/26306585)

[Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

## 参见

- [String常用操作](https://www.jianshu.com/p/798d822710b4)
- [Swift String与NSString的区别](https://www.jianshu.com/p/0a0511a6d0de)