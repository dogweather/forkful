---
date: 2024-01-20 17:51:53.163763-07:00
description: 'How to: Swift makes string interpolation a breeze with the `\(variableName)`
  syntax.'
lastmod: '2024-03-13T22:45:00.383433-06:00'
model: gpt-4-1106-preview
summary: Swift makes string interpolation a breeze with the `\(variableName)` syntax.
title: Interpolating a string
weight: 8
---

## How to:
Swift makes string interpolation a breeze with the `\(variableName)` syntax.

```Swift
let name = "Jane"
let age = 28
let greeting = "Hello, \(name), you are \(age) years old."
print(greeting)  // Output: Hello, Jane, you are 28 years old.
```

You can even perform operations within the interpolation:

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "I have \(apples + oranges) pieces of fruit."
print(fruitSummary)  // Output: I have 8 pieces of fruit.
```

## Deep Dive
Okay, let's get a bit historical. String interpolation isn't unique to Swift. It exists in many languages (like JavaScript, Python, etc.), but Swift's version is type-safe meaning the compiler checks the types for you, reducing errors.

Before Swift 5, string interpolation was less powerful and more cumbersome. But Swift 5 introduced Extended String Interpolation, which allows you to customize string interpolation, bringing impressive flexibility.

Alternatives to string interpolation in Swift include concatenation using `+`, and the older `String(format:)` method. However, these are less convenient and, for format strings, harder to read.

Implementation details? With Swift's string interpolation, you can customize how types are represented within strings by extending the `StringInterpolation` protocol. This means you can define how custom types are displayed during interpolation, which is super handy.

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ value: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: value))
    }
}

let today = Date()
let dateString = "Today's date is \(today)."
print(dateString) // Output will be today's date in medium style formatting.
```

## See Also
To get the nitty-gritty on string interpolation, Swift's documentation is golden:
- [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Swift Evolution Proposal for Improved String Interpolation](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

For swirling deeper into formatting custom types:
- [Customizing String Interpolation in Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
