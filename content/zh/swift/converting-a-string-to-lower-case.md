---
title:    "Swift: 将字符串转换为小写"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在Swift编程中，我们经常会遇到需要将字符串转换为小写的情况。这可以帮助我们统一字符串的格式，方便后续操作。接下来，我们将深入了解如何在Swift中将字符串转换为小写。

## 如何

```Swift
let name = "JOHN DOE"
print(name.lowercased())
```

```Swift
// Output: john doe
```

我们可以通过使用字符串的内置方法`lowercased()`来将字符串转换为小写。这个方法会返回一个新的字符串，而原来的字符串不会被改变。

为了更好地理解，我们可以通过以下代码来比较转换前后的字符串：

```Swift
let name = "JOHN DOE"
print(name.lowercased())
print(name)
```

```Swift
// Output:
// john doe
// JOHN DOE
```

从上面的输出可以看出，使用`lowercased()`方法后，新的字符串被打印为小写，但原始的字符串仍然保持不变。

## 深入了解

在Swift中，字符串是一个值类型，这意味着它们是不可变的。因此，当我们需要更改字符串时，实际上是创建了一个新的字符串，并将其赋值给原始字符串的变量或常量。

在字符串转换为小写的过程中，Swift会使用Unicode标准来处理字符。通常情况下，大写字母转换为小写字母的方式是通过在ASCII值上加上32来实现。但是，有些字符的转换方式可能会有所不同，这取决于它们在Unicode中的位置。

此外，我们还可以使用`.uppercased()`方法来将字符串转换为大写，以及` .capitalized`方法来将字符串的首字母转换为大写。

## 参考链接

- [The Swift Programming Language: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String.lowercased()](https://developer.apple.com/documentation/swift/string/2995556-lowercased)
- [Unicode.org](https://unicode.org)

## 参见