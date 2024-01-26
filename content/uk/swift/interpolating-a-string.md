---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:37.457758-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
String interpolation allows us to insert variables into strings. It's a swift, clean way to construct strings - essentials in coding.

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
