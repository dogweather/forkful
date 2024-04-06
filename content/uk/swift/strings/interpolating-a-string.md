---
date: 2024-01-20 17:51:37.457758-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) With this, you can also do calculations or call functions within the placeholders."
lastmod: '2024-04-05T21:53:49.972991-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ With this, you can also do calculations or call functions within the placeholders."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

## How to: (Як це зробити:)
```Swift
let name = "Олексій"
let age = 29
let greeting = "Привіт, мене звати \(name) і мені \(age) років."
print(greeting) // Виводить: Привіт, мене звати Олексій і мені 29 років.
```

With this, you can also do calculations or call functions within the placeholders:
```Swift
let price = 1299.99
let taxRate = 0.2
let priceWithTax = "Ціна з ПДВ: \(price * (1 + taxRate))"
print(priceWithTax) // Виводить: Ціна з ПДВ: 1559.988
```

## Deep Dive (Поглиблений Розбір):
Initially, programmers concatenated strings using `+`, but this got unwieldy. Swift's string interpolation is more intuitive. Interpolation is more than just variables—it's any valid expression, including function calls or calculations.

Alternatives like `String(format:)` exist, used mostly for formatting strings comprehensively (similar to `printf` in C).

How the interpolation works: Swift compiles your interpolated string into a series of appends to `String`. This is efficient, but there's a performance consideration with very large strings or intense use.

## See Also (Дивіться також):
- Swift Documentation on String Interpolation: [Strings and Characters — The Swift Programming Language (Swift 5.7)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial on Swift’s String Interpolation: [RW Tutorials](https://www.raywenderlich.com/)
- Swift API for `String`: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
