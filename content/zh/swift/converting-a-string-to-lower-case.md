---
title:    "Swift: 将字符串转换为小写"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将字符串转换为小写，这可以帮助我们在比较和匹配字符串时更加精确，避免由于大小写不一致而导致的错误。

## 如何操作

操作很简单，我们可以使用 `.lowercased()` 方法将字符串转换为小写。例如：

```Swift
let str = "Today is a Great Day!"
print(str.lowercased())

// 输出：today is a great day!
```

我们也可以使用 `.components(separatedBy: "")` 方法将字符串按照指定的字符分割为数组，然后使用 `joined()` 方法将数组中的字符串连接起来，并使用 `.lowercased()` 方法将其转换为小写。例如：

```Swift
let str = "Hello, my name is John"
let arr = str.components(separatedBy: " ")
let newStr = arr.joined(separator: " ")
print(newStr.lowercased())

// 输出：hello, my name is john
```

## 深入了解

在 Swift 中，字符串属于值类型，也就是说每次操作都会返回一个新的字符串，而不会改变原始字符串。因此，当我们使用 `.lowercased()` 方法时，实际上我们得到的是一个新的小写字符串，而原始字符串仍然保持不变。另外，我们也可以使用 `.uppercased()` 方法将字符串转换为大写。同时，我们还可以使用 `.capitalized` 方法将字符串的首字母转换为大写。例如：

```Swift
let str = "hello, my name is John"
print(str.lowercased()) // 输出：hello, my name is john
print(str.uppercased()) // 输出：HELLO, MY NAME IS JOHN
print(str.capitalized) // 输出：Hello, My Name Is John
```

## 参考链接

- [Swift String and Character](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)