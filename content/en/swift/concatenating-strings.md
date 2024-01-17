---
title:                "Concatenating strings"
html_title:           "Swift recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is the process of combining two or more strings into one. It allows programmers to create longer strings from shorter ones, which is useful when dealing with dynamic content or user input. This technique is commonly used in text processing, data manipulation, and user interface development.

## How to:
Concatenating strings in Swift is simple and straightforward. You can use the `+` operator or the `+=` compound assignment operator to join two strings together. Here's an example:

```Swift
let firstName = "John"
let lastName = "Smith"
let fullName = firstName + " " + lastName
print(fullName) // Output: "John Smith"
```

You can also use string interpolation to combine constants, variables, and expressions into a single string. Simply wrap the desired values with `\()` inside a string. Here's an example:

```Swift
let age = 25
let info = "I am \(age) years old."
print(info) // Output: "I am 25 years old."
```

## Deep Dive:
The concept of concatenating strings has been around since the early days of computing. It originated from the need to combine multiple pieces of text into a single continuous string. Prior to the `+` operator, concatenating strings in Swift was done using the `append()` method, which is still available for use.

In addition to using `+` and `+=` operators, you can also use the `join()` method to concatenate an array of strings. This method adds a separator in between each string, giving you more control over the output. Here's an example:

```Swift
let fruits = ["apple", "banana", "orange"]
let joinedFruits = fruits.joined(separator: ", ")
print(joinedFruits) // Output: "apple, banana, orange"
```

Apart from concatenating strings, there are other techniques that programmers can use to manipulate and format strings in Swift. These include string interpolation, string manipulation methods like `uppercased()` and `lowercased()`, and the use of format specifiers for formatting numerical and date values within a string.

## See Also:
To learn more about string concatenation in Swift, check out the following resources:

- [Strings and Characters - The Swift Programming Language](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String Interpolation - Hacking with Swift](https://www.hackingwithswift.com/syntax/86-string-interpolation)
- [Building Strings from Multiple Components - Swift by Sundell](https://www.swiftbysundell.com/articles/building-strings-in-swift-from-multiple-components/)