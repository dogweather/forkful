---
title:                "Interpolating a string"
html_title:           "Swift recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string means inserting variables or expressions into a string to create a new string. Programmers often do this to dynamically create unique strings based on the current values of variables or the result of expressions. It allows for more flexible and efficient string manipulation in code.

## How to:
To interpolate a string in Swift, we use the backslash and parentheses notation ```\(variable/expression)``` within a string. An example use case could be creating a personalized message using a user's name as a variable:

```Swift
let name = "John"
let message = "Hello \(name), welcome to our app!"
```

The output of the message variable would be "Hello John, welcome to our app!" 

## Deep Dive:
Historically, string interpolation was not always a common technique in programming languages. In fact, it was only added as a feature to Swift in 2014 with the release of Swift 1.0. Before this, developers had to manually concatenate strings and variables, which could be tedious and error-prone.

An alternative to string interpolation is using string formatting, which is a more verbose method that requires using special symbols and placeholders to indicate where variables or expressions should be inserted into a string.

In terms of implementation, Swift's string interpolation works by converting the variable or expression into a string format and inserting it into the string at runtime. This process is optimized for performance, making it a preferred method for string manipulation in Swift.

## See Also:
To learn more about string interpolation in Swift, check out Apple's official documentation on the topic: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID661