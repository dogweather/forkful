---
date: 2024-01-20 17:58:47.455822-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) Back in\
  \ the day, text operations were cumbersome in programming languages, but Swift made\
  \ them a breeze. Swift's String class has\u2026"
lastmod: '2024-04-05T22:51:02.820721-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) Back in the day, text\
  \ operations were cumbersome in programming languages, but Swift made them a breeze."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## How to: (Як робити:)
```Swift
let originalText = "Hello, World!"
let searchText = "World"
let replaceText = "Ukraine"

if let range = originalText.range(of: searchText) {
    let replacedText = originalText.replacingCharacters(in: range, with: replaceText)
    print(replacedText) // Output: Hello, Ukraine!
} else {
    print("Search text not found.")
}
```

## Deep Dive (Глибше Занурення)
Back in the day, text operations were cumbersome in programming languages, but Swift made them a breeze. Swift's String class has rich functionality for search and replace, using methods like `range(of:)` and `replacingCharacters(in:with:)`. Alternatives? Sure – regular expressions are powerful for complex patterns, and text frameworks provide more features. As for details – Swift's strings are Unicode-compliant, so search-and-replace works smoothly even with diverse alphabets and emojis.

## See Also (Дивіться також):
- Swift's String documentation: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- Regular Expressions in Swift: [https://nshipster.com/swift-regular-expressions/](https://nshipster.com/swift-regular-expressions/)
- Working with Strings in Swift: [https://www.hackingwithswift.com/articles/108/how-to-use-strings-in-swift](https://www.hackingwithswift.com/articles/108/how-to-use-strings-in-swift)
