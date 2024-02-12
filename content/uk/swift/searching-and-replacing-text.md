---
title:                "Пошук та заміна тексту"
aliases:
- uk/swift/searching-and-replacing-text.md
date:                  2024-01-20T17:58:47.455822-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Searching and replacing text lets you find strings within strings and swap them with something else. Programmers do it for tasks like editing user input, processing data, or manipulating text files – basically, to make life easier when dealing with lots of text.

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
