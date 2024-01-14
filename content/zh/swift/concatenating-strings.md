---
title:                "Swift: 串接字串"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

字符串连接是一种常见的编程操作，可以将多个短字符串连接成一个长字符串。它可以帮助我们更轻松地处理文本数据，比如拼接用户的名字和姓氏来创建欢迎消息。

## 如何操作

我们可以使用 "＋" 运算符来连接两个字符串，如下所示：

```Swift
let firstName = "Janet"
let lastName = "Li"
let fullName = firstName + " " + lastName
```

这将把 "Janet" 和 "Li" 连接起来并创建一个新的字符串 "Janet Li"。我们也可以在 Swift 中使用 `string interpolation` 来简单地将变量和字符串组合起来，如下所示：

```Swift
let age = 25
let message = "你的年龄是：\(age)"
print(message) // 输出：你的年龄是：25
```

## 深入了解

值得注意的是，字符串连接会在内存中创建新的字符串，并将原来的字符串拷贝到新的字符串中。如果需要频繁地进行字符串连接，这可能会导致性能问题。为了避免这种情况，我们可以使用 `String` 类型的 `append()` 方法来逐步创建一个新的字符串，而不是每次都在内存中创建新的字符串。

## 参考链接

- [Swift 文档：字符串与字符](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) 
- [连接 Swift 字符串的不同方式](https://useyourloaf.com/blog/swift-string-cheat-sheet/)
- [Swift 中的字符串操作技巧](https://www.hackingwithswift.com/articles/141/6-unique-ways-to-work-with-strings-in-swift)
- [String 类型文档](https://developer.apple.com/documentation/swift/string)