---
title:                "Using regular expressions"
html_title:           "Swift recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Using regular expressions in Swift is a way for programmers to search, match, and manipulate text using pattern matching. This can be useful for tasks such as data validation, parsing, and text replacement.

## How to:

```Swift
// Example 1: Matching a specific string
let string = "The quick brown fox jumps over the lazy dog"
let pattern = "fox"
let regex = try NSRegularExpression(pattern: pattern)
if let match = regex.firstMatch(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count)) {
    print("Found match: \(string.substring(with: match.range))") // Output: "fox"
}

// Example 2: Matching a phone number
let phoneNumber = "123-456-7890"
let pattern = "^[0-9]{3}-[0-9]{3}-[0-9]{4}$"
let regex = try NSRegularExpression(pattern: pattern)
if regex.matches(phoneNumber) {
    print("Valid phone number!") // Output: "Valid phone number!"
}
```

## Deep Dive:

Regular expressions have been around since the 1950s, but it wasn't until the 1960s and 1970s with the development of Unix tools such as grep and sed that they became popular. In Swift, regular expressions can be used with the NSRegularExpression class from Foundation framework.

An alternative to using regular expressions is the Swift "range(of:)" method, which can also search for a string within another string. However, regular expressions offer more flexibility and control over the search pattern.

Under the hood, regular expressions use a technique called "finite state machine" or "DFA" (deterministic finite automaton) to efficiently search through text. This is much faster than manually searching for a match character by character.

## See Also:

- NSRegularExpression Documentation: https://developer.apple.com/documentation/foundation/nsregularexpression
- Regular Expressions 101 (online regex tester): https://regex101.com/
- Swift Strings and Characters Guide: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID290