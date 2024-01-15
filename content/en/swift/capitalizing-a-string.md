---
title:                "Capitalizing a string"
html_title:           "Swift recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a trivial task, but it can actually have important use cases in programming. It can make user input more consistent, improve the readability of text, and even comply with specific formatting requirements for data reports or presentations.

## How To

In Swift, capitalizing a string can be achieved using the `String` method `capitalized` or `capitalizedFirstLetter()`. Let's take a look at some code examples:

```Swift
let name = "john"
let capitalized = name.capitalized
print(capitalized) // Prints "John"

let sentence = "this is a sentence."
let capitalizedFirst = sentence.capitalizedFirstLetter()
print(capitalizedFirst) // Prints "This is a sentence."
```

In the first example, we used the `capitalized` method to capitalize the first letter of the string "john". In the second example, we used the `capitalizedFirstLetter()` method to capitalize the first letter of each word in the string "this is a sentence."

Keep in mind that these methods will only capitalize the first letter of each word in the string, not the entire string itself.

## Deep Dive

The `String` method `capitalized` is based on the current locale of the device, meaning it will follow the capitalization rules of the user's language and location. This can be useful when working with international users or creating localized content.

Additionally, the `capitalized` method can also take in a parameter specifying the locale to be used. This can be helpful in situations where you need to ensure consistent capitalization across different devices or platforms.

## See Also

- [Apple's documentation on the `capitalized` method](https://developer.apple.com/documentation/swift/string/3126871-capitalized)
- [A tutorial on capitalizing strings in Swift](https://www.hackingwithswift.com/quick-start/swiftui/how-to-capitalize-the-first-letter-of-a-string)
- [Using Swift's `capitalized` method with localization](https://useyourloaf.com/blog/localized-string-capitalization-in-swift/)