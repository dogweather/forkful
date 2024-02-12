---
title:                "字符串插值"
aliases:
- /zh/swift/interpolating-a-string.md
date:                  2024-01-20T17:51:57.398306-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/interpolating-a-string.md"
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
