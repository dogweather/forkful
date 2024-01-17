---
title:                "插入字符串"
html_title:           "Kotlin: 插入字符串"
simple_title:         "插入字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串插值？

字符串插值是一种编程技术，它允许程序员将变量或表达式直接嵌入到字符串中。这样做可以更方便地构建动态的字符串，而无需手动拼接（concatenate）它们。

## 如何使用：

### 基本用法：

```
val name = "张三"
val greeting = "你好，$name！" 
println(greeting) //输出：你好，张三！
```

### 括号可选：

```
val num1 = 10
val num2 = 5
val result = "${num1}加上${num2}等于${num1 + num2}"
println(result) //输出：10加上5等于15
```

## 深入了解：

### 历史背景：

字符串插值在2011年由Swift语言提出，随后被众多编程语言采纳，包括Kotlin。它的出现使得动态字符串的构建变得更加简单和直观。

### 其他替代方法：

在早期，拼接字符串是一种常用的方法，但它会导致代码可读性差和维护困难。另外，使用Java语言时，可以通过String.format()方法来实现类似的功能，但它相对复杂且易出错。

### 实现细节：

字符串插值通过在字符串前加上"$"符号来实现，在运行时编译器会将字符串拆分为静态部分和变量/表达式。然后使用StringBuilder类来构建最终的字符串，这也是它的效率比拼接字符串高的原因。

## 查看更多：

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation) 
- [Swift官方文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)