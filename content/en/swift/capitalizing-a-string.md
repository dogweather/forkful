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

# What & Why?

Capitalizing a string means converting the first character of the string to uppercase, and leaving the rest of the characters as is. Programmers do this to standardize the appearance of their output and to make it more readable.

# How to:

Here is an example of how to capitalize a string in Swift:

```Swift
var exampleString = "hello world"
exampleString.capitalized  // Output: "Hello world"
```

You can also make the first letter of every word in a string uppercase, while keeping the rest lowercase, by using the `capitalized(with:)` method. Here's an example:

```Swift
var multiWordString = "this is a multi-word string"
multiWordString.capitalized(with: Locale(identifier: "en_US")) // Output: "This Is A Multi-Word String"
```

# Deep Dive

Capitalizing strings has been a common practice in programming since the early days of computing. It helps to maintain consistency and readability in code and output. Before Swift, languages like C and Java had built-in functions for capitalizing strings. In Swift, the `capitalized` and `capitalized(with:)` methods were introduced in version 2.0.

There are alternative ways to capitalize strings in Swift, such as using the `uppercased()` and `lowercased()` methods or creating a custom function. However, the `capitalized` and `capitalized(with:)` methods are the most convenient and efficient options.

When using the `capitalized(with:)` method, the language and cultural conventions can affect the output. In the example above, we used the "en_US" locale identifier to ensure that the string follows the conventions of American English. You can use "en_UK" for British English, "fr_FR" for French, "de_DE" for German, etc.

# See Also

To learn more about manipulating strings in Swift, you can check out the official documentation on Strings and Characters: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html