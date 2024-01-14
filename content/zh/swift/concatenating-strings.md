---
title:    "Swift: 连接字符串"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

在Swift编程中，连接字符串是一个非常常见的操作。它允许我们将多个字符串组合成一个更大的字符串，这在处理文本和用户输入时非常有用。接下来，让我们来看看如何使用Swift来连接字符串。

## 为什么

连接字符串是一种很有用的技术，因为它允许我们创建一个更大的字符串，而不需要手动输入每个字符。这节省了大量的时间和努力，并使我们的代码更容易阅读和维护。

## 如何

在Swift中，连接字符串可以通过使用"+"操作符来实现。让我们看一个简单的例子，并将两个字符串“Aloha”和“世界”连接起来。

```Swift
let greeting = "Aloha" + "世界"
print(greeting)
```

输出应该为：Aloha世界

我们也可以使用字符串插值来连接字符串。这允许我们在字符串中嵌入变量或表达式，并将其转换为字符串。让我们看一个例子：

```Swift
let name = "小明"
let greeting = "你好，\(name)"
print(greeting)
```

输出应为：你好，小明

## 深入探讨

在Swift中，字符串连接使用的是字符串拷贝，这意味着原始字符串不会被修改，而是创建一个新的字符串。因此，在对性能要求较高的情况下，我们可以使用Swift中的其他方法来连接字符串，如使用`append()`方法或将字符串放入数组中并使用`joined()`方法。

此外，字符串连接也可以与for循环结合使用，以便将多个字符串连接起来。让我们看一个例子：

```Swift
let fruits = ["苹果", "香蕉", "橘子"]
var result = ""
for fruit in fruits {
    result += fruit + "，"
}
print(result)
```

输出应为：苹果，香蕉，橘子，

## 参考链接

- [Swift字符串官方文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift字符串拼接方法](https://www.hackingwithswift.com/articles/162/how-to-join-strings-free-using-appending-or-joined)
- [使用+操作符连接字符串](https://www.swiftbysundell.com/basics/string-interpolation-and-concatenation/)
- [参考字符串操作方法](https://www.programiz.com/swift-programming/methods/string)

## 参见

- [Swift中的字符串操作技巧](https://www.appcoda.com/swift4-changes/)
- [用Swift连接字符串的方法](https://www.hackingwithswift.com/articles/162/how-to-join-strings-free-using-appending-or-joined)