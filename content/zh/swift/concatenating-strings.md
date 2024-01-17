---
title:                "拼接字符串"
html_title:           "Swift: 拼接字符串"
simple_title:         "拼接字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

#什么是字符串连接？
字符串连接是指将多个字符串合并在一起成为一个新的字符串的过程。程序员们通常会使用字符串连接来构建动态的、可变的信息，例如在登录页面上显示用户的姓名和欢迎信息等等。

#为什么要进行字符串连接？
在写程序过程中，我们经常需要将多个字符串连接起来来构建新的信息。比如，我们可能需要将用户输入的用户名和密码连接在一起来验证用户身份。通过字符串连接，我们可以轻松地将多个信息组合起来创建出我们需要的信息。

#如何进行字符串连接？
```
let str1 = "Hello"
let str2 = "World"
let str3 = str1 + str2
print(str3)
```
输出结果为：HelloWorld

#深入学习
在历史上，字符串连接是一项非常重要的技术，因为它使得程序员们可以动态地操作和构建字符串信息。除了使用"+"符号来进行连接，还有其他一些替代方案，例如使用字符串插值来构建字符串。在实现字符串连接时，需要注意的是对空字符串的处理，以及对不同编码格式的支持。

#相关阅读
- [字符串插值 vs 字符串连接](https://www.objc.io/blog/2018/02/27/string-interpolation-vs-string-concatenation/)
- [Swift语言官方文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID287)