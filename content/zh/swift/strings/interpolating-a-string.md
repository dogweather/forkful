---
date: 2024-01-20 17:51:57.398306-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662FSwift\u7F16\u7A0B\u4E2D\u5C06\u53D8\
  \u91CF\u3001\u5E38\u91CF\u3001\u5B57\u9762\u91CF\u6216\u8868\u8FBE\u5F0F\u63D2\u5165\
  \u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u65B9\u4FBF\u5730\u6784\u9020\u5B57\u7B26\u4E32\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.944969-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662FSwift\u7F16\u7A0B\u4E2D\u5C06\u53D8\u91CF\
  \u3001\u5E38\u91CF\u3001\u5B57\u9762\u91CF\u6216\u8868\u8FBE\u5F0F\u63D2\u5165\u5230\
  \u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u65B9\u4FBF\u5730\u6784\u9020\u5B57\u7B26\u4E32\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
字符串插值是Swift编程中将变量、常量、字面量或表达式插入到字符串中的过程。程序员这样做是为了方便地构造字符串。

## How to (怎么做)
```Swift
// 基础用法
let name = "王小明"
let greeting = "你好, \(name)!"
print(greeting)  // "你好, 王小明!"

// 表达式使用
let price = 45
let quantity = 3
let message = "总价: \(price * quantity)元"
print(message)  // "总价: 135元"

// 格式化
let pi = 3.14159
let formattedPi = String(format: "π的值是: %.2f", pi)
print(formattedPi)  // "π的值是: 3.14"
```

## Deep Dive (深入探讨)
字符串插值在Swift初版引入，旨在替代传统的字符串格式化方法。它更直观，几乎可以嵌入任何类型的实例，只要该类型遵循`CustomStringConvertible`协议。

对比旧方法，如Objective-C中的`NSString stringWithFormat:`，Swift的插值更简洁，易读。但注意，过度使用可能降低代码清晰度。

在底层，当你在字符串中使用`\()`插入表达式时，Swift调用`String`类型的`init(stringInterpolation:)`构造器来创建新的字符串实例。

## See Also (另请参阅)
- [Swift官方字符串插值文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift标准库字符串API](https://developer.apple.com/documentation/swift/string)
