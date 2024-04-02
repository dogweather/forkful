---
date: 2024-02-03 19:02:38.526736-07:00
description: "Capitalizing a string in Swift modifies the given string so that its\
  \ first character is uppercase, and the remaining characters are lowercase. Programmers\u2026"
lastmod: '2024-03-13T22:45:00.380840-06:00'
model: gpt-4-0125-preview
summary: "Capitalizing a string in Swift modifies the given string so that its first\
  \ character is uppercase, and the remaining characters are lowercase. Programmers\u2026"
title: Capitalizing a string
weight: 2
---

## What & Why?

Capitalizing a string in Swift modifies the given string so that its first character is uppercase, and the remaining characters are lowercase. Programmers do this for purposes like formatting names or sentences according to grammatical rules or user interface standards.

## How to:

Swift's `String` structs come with a couple of built-in methods to manipulate the case of strings. Here are a few approaches to capitalize strings in Swift, including the use of standard methods and third-party libraries if necessary.

### Using built-in methods

To capitalize the first letter of a string and lowercasing the rest:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Output: "Hello, world"
```

To capitalize the first letter of each word in a sentence, you can use the `capitalized` property:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Output: "Hello, World"
```

### Using a third-party library

While Swift's standard library is quite comprehensive, some specific capitalization formats might require more complex operations or can be simplified using third-party libraries. One of the popular ones for string manipulation is SwiftRichString. (Note: Always ensure to include third-party libraries through Swift Package Manager, CocoaPods, or Carthage, and import them in your file.)

First, you would need to add `SwiftRichString` to your project. Once installed, you can use it to perform various string operations, including specific capitalization needs. However, as of now, Swift's built-in methods adequately cover most capitalization use cases without needing external libraries for just capitalizing strings.

Always refer to the latest documentation of the library for any updates or changes in methods.
