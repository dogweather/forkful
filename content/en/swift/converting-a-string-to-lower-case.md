---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# All You Need to Know About Swift's Lowercase String Method

## What & Why?

In Swift programming, converting a string to lower case is changing all the alphabetical characters in the string from uppercase to lowercase. Programmers do this to standardize and simplify the handling of text data, or implement case-insensitive comparisons.

## How to:

Converting a string to lower case is straightforward in Swift. Here's how:

```Swift
let sentence = "HELLO, WORLD!"
let lowercasedSentence = sentence.lowercased()
print(lowercasedSentence)
```

The output you'll get will be:

```Swift
"hello, world!"
```

## Deep Dive

Swift, developed by Apple, consistently improves on readability and simplicity. The `lowercased()` method is part of the standard Swift library, and it reflects this focus.

Historically, case manipulation in programming languages was complex, often involving ASCII calculations or cumbersome function calls. Swift's `lowercased()` is much more straightforward.

An alternative to `lowercased()` is `localizedLowercase`, which takes current locale settings into account. "HELLO, WORLD!".localizedLowercase outputs "hello, world!", but its results may vary in different locales that have different rules for lowercasing.

Under the hood, `lowercased()` and `localizedLowercase` work by iterating over each Character of the String and transforming it according to Unicode case mapping rules. The mapping is many-to-one, meaning uppercase versions of a single character can vary and still map to the same lower case character.

## See Also:

Detailed information related to Swift's string manipulation can be found in these sources:

1. [String and Character operations - Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Swift Basic Operators](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
3. [Unicode Case Mappings](https://unicode.org/charts/case/)
4. ["LowercaseStrings", a coding challenge on the Swift Algorithm Club](https://www.raywenderlich.com/developers/swift-algorithm-club)
   
Happy coding!