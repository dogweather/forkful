---
title:                "Converting a string to lower case"
html_title:           "Swift recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case simply means changing all the letters in a string to their lower case equivalents. This is commonly done by programmers to ensure consistency in data and avoid any errors that can occur due to case-sensitivity.

## How to:

To convert a string to lower case in Swift, there are a few different methods you can use depending on your specific needs. Here are two examples:

````Swift
let sentence = "Hello, World!"
let lowerCaseSentence = sentence.lowercased()
print(lowerCaseSentence)
//Output: hello, world!
````

You can also use the `String` method `localizedLowercase` to handle special Unicode characters:

````Swift
let heart = "\u{1F496}" // Unicode character for heart
let string = "I ❤️ Swift"
let lowerCaseString = string.localizedLowercase
print(lowerCaseString)
//Output: i ❤️ swift
````

## Deep Dive:

Converting a string to lower case is a common practice in programming languages, with roots going back to the early days of ASCII character encoding. Before coding standards and protocols were established, case-sensitivity could often lead to inconsistent data and errors. Today, lower casing is still commonly used in programming for data standardization and to avoid errors.

There are alternative methods to convert a string to lower case, such as using string manipulation functions or regular expressions. However, these may require more code and can be less efficient than using the built-in methods in Swift.

Internally, Swift uses the Unicode standard for character encoding, which supports both upper and lower case letters for most languages. This allows for easy conversion between cases, making it a practical and efficient method.

## See Also:

- [Apple's Documentation on String](https://developer.apple.com/documentation/swift/string)
- [Swift.org's Tutorial on Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode Official Website](https://home.unicode.org/)