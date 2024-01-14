---
title:                "Swift recipe: Searching and replacing text"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text may seem like a simple task, but it can save you a lot of time and effort in your Swift programming endeavors. With the right techniques, you can easily make changes to multiple instances of text in your code without having to manually edit them one by one.

## How To

To start, we will define a string variable that contains the text we want to search and replace:

```Swift
let originalString = "Hello, World!"
```

Next, we will use the `replacingOccurrences(of:with:)` method to search for a specific word or phrase and replace it with another text. For example, if we want to replace "Hello" with "Hi", we can do it like this:

```Swift
let modifiedString = originalString.replacingOccurrences(of: "Hello", with: "Hi")
```

The value of `modifiedString` will now be "Hi, World!". But what if we want to replace multiple instances of a word or phrase? We can use the same `replacingOccurrences(of:with:)` method, but this time with the `options:range:` parameter. Let's say we want to replace all instances of the letter "l" with the number "1" in our string, we can do it like this:

```Swift
let convertedString = originalString.replacingOccurrences(of: "l", with: "1", options: .literal, range: nil)
```

The value of `convertedString` will now be "He11o, Wor1d!". You can also specify a range where you want the replacements to occur using the `range` parameter.

## Deep Dive

The `replacingOccurrences(of:with:)` method uses regular expressions to find and replace text. This means we can also use regex patterns to make more complex search and replace operations. For example, if we want to replace all numbers in our string with the word "number", we can do it like this:

```Swift
let regexPattern = "\\d+"
let replacedNumbers = originalString.replacingOccurrences(of: regexPattern, with: "number", options: .regularExpression, range: nil)
```

The value of `replacedNumbers` will now be "Hello, number!". By diving deeper into regular expressions, we can perform even more advanced search and replace tasks in our Swift code.

## See Also

If you want to learn more about working with strings in Swift, here are some helpful resources:

- [Working with Strings in Swift](https://www.hackingwithswift.com/read/0/overview)
- [NSHipster - String](https://nshipster.com/string/)
- [Apple Developer Documentation - String](https://developer.apple.com/documentation/swift/string)