---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串插值是一种构建新字符串值的方式，能在其中包含常量、变量、字面值和表达式的值。程序员使用它来直观、清晰地定义和控制文本数据的结构，使代码更易于阅读和理解。

## 如何操作：

Swift 中字符串插值的标准语法：

```Swift
let name = "张三"
let greeting = "你好，\(name)！" 
print(greeting) 
```
输出：

```Swift
你好，张三！
```

在 `\( ... )` 内部，我们可以插入代码。

```Swift
let apples = 3
print("我有\(apples)个苹果。")
```
输出：

```Swift
我有3个苹果。
```
## 深入剖析

1. **历史背景**：Swift 在 1.0 版本引入了字符串插值，现在它成为标准的字符串操作方式。
2. **替代方案**：在 Swift 之前的编程语言中，我们常常使用 `+` 操作符连接字符串，或使用格式说明符，例如 `%@`，然后调用 `String(format:args:)` 方法。
3. **实现细节**：Swift 是通过将运算符 `+` 重载为字符串连接操作符来实现字符串插值的。在Swift中，有效的插值仅限于在 `\( ... )` 中可以求值的表达式。

## 参考资料

欲了解更多关于 Swift 的字符串插值，可以访问官方文档：
https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html 

有关 Swift 字符串的详细讲解，可以查看这篇博客：
https://www.hackingwithswift.com/articles/162/how-to-use-string-interpolation-in-swift