---
date: 2024-01-20 17:51:57.398306-07:00
description: "How to (\u600E\u4E48\u505A) \u5B57\u7B26\u4E32\u63D2\u503C\u5728Swift\u521D\
  \u7248\u5F15\u5165\uFF0C\u65E8\u5728\u66FF\u4EE3\u4F20\u7EDF\u7684\u5B57\u7B26\u4E32\
  \u683C\u5F0F\u5316\u65B9\u6CD5\u3002\u5B83\u66F4\u76F4\u89C2\uFF0C\u51E0\u4E4E\u53EF\
  \u4EE5\u5D4C\u5165\u4EFB\u4F55\u7C7B\u578B\u7684\u5B9E\u4F8B\uFF0C\u53EA\u8981\u8BE5\
  \u7C7B\u578B\u9075\u5FAA`CustomStringConvertible`\u534F\u8BAE\u3002 \u5BF9\u6BD4\
  \u65E7\u65B9\u6CD5\uFF0C\u5982Objective-C\u4E2D\u7684`NSString\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.433498-06:00'
model: gpt-4-1106-preview
summary: "\u5BF9\u6BD4\u65E7\u65B9\u6CD5\uFF0C\u5982Objective-C\u4E2D\u7684`NSString\
  \ stringWithFormat:`\uFF0CSwift\u7684\u63D2\u503C\u66F4\u7B80\u6D01\uFF0C\u6613\u8BFB\
  \u3002\u4F46\u6CE8\u610F\uFF0C\u8FC7\u5EA6\u4F7F\u7528\u53EF\u80FD\u964D\u4F4E\u4EE3\
  \u7801\u6E05\u6670\u5EA6."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
