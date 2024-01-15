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

## Why

Converting a string to lower case is a common task in programming that allows for easier manipulation and comparison of strings. It can be useful for tasks such as data validation, sorting, and searching within a string.

## How To

To convert a string to lower case in Swift, you can use the built-in `lowercased()` method. Here is an example:

```
let string = "Hello World"
let lowercasedString = string.lowercased()
print(lowercasedString)

// Output: "hello world"
```

We simply call the `lowercased()` method on our string and assign the result to a new variable. Note that this method returns a new string, so we cannot modify the original string in place.

Another option is to use the `uppercased()` method and then convert it to lower case using the `lowercased()` method. Here is an example:

```
let string = "Hello World"
let lowercasedString = string.uppercased().lowercased()
print(lowercasedString)

// Output: "hello world"
```

This approach may be useful if you need to change the case of a string multiple times in your code.

## Deep Dive

Internally, Swift uses the Unicode standard for case conversions. This means that the `lowercased()` method will also handle special characters and symbols, not just the traditional English alphabet. For example:

```
let string = "Séléna Gómez"
let lowercasedString = string.lowercased()
print(lowercasedString)

// Output: "séléna gómez"
```

Additionally, Swift has another method called `localizedLowercase` which takes into account language and regional conventions for case mapping. This can be useful if your app supports multiple languages.

Overall, converting a string to lower case may seem like a simple task, but it is important to understand how it works internally and the potential impact on different languages and character sets.

## See Also

- Swift String documentation: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Unicode case mapping: https://unicode.org/reports/tr12/