---
title:                "字符串连接"
html_title:           "Swift: 字符串连接"
simple_title:         "字符串连接"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么要连接字符串

当我们在编写代码时，有时候需要将多个字符串拼接成为一个更长的字符串。比如说，在用户注册时，我们需要将用户的名字和姓氏拼接在一起，生成一个完整的姓名。这就是为什么我们需要掌握字符串拼接(concatenation)的方法。

## 如何拼接字符串

拼接字符串可以使用加号 "+" 来实现，如下所示：

```Swift
var fName = "Tom"
var lName = "Hanks"

var fullName = fName + " " + lName  // "Tom Hanks"

print(fullName)  // 输出：Tom Hanks
```

需要注意的是，加号操作符不能直接用于将字符串和其他类型的值拼接起来。我们需要使用字符串插值(interpolation)来完成这一操作，如下所示：

```Swift
var age = 23
var intro = "我今年\(age)岁了。"

print(intro)  // 输出：我今年23岁了。
```

除了使用加号和字符串插值，我们还可以使用 `String` 类的 `append()` 方法来拼接字符串，如下所示：

```Swift
var greeting = "Hello, "

greeting.append("world!")

print(greeting) // 输出：Hello, world!
```

## 深入了解字符串拼接

在 Swift 中，字符串的拼接可以通过两种方式来实现：使用 `+` 操作符拼接和使用 `append()` 方法拼接。但实际上，这两种方法的背后都是通过使用 `String` 类的 `append()` 方法来执行的。在拼接较长的字符串时，推荐使用 `append()` 方法，因为它的性能会比直接使用 `+` 操作符要好。

此外，我们还可以使用 `String` 类的 `joined(separator:)` 方法来将多个字符串拼接在一起，中间使用指定的 `separator` 进行分隔，如下所示：

```Swift
var fruits = ["apple", "banana", "orange"]

var str = fruits.joined(separator: ", ") // str = "apple, banana, orange"
```

总的来说，拼接字符串是我们在日常开发中经常使用的操作，掌握多种拼接方法，可以让我们的代码更加简洁高效。

## 查看更多

- [Swift 字符串拼接方法](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID286)
- [String 类的 append() 方法](https://developer.apple.com/documentation/swift/string/1540086-append)
- [String 类的 joined(separator:) 方法](https://developer.apple.com/documentation/swift/string/1540079-joined)