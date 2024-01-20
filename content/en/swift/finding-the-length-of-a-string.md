---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string refers to determining the number of characters in it. Programmers do it to control text input in UI design, validate user input, or manipulate strings better.

## How to:

In Swift, to find out the length of a string, we simply use the `count` property, like so:

```Swift
let myString = "Hello, Swift!"
print(myString.count)
```

This will output:

`13`

The count includes the characters, spaces, and punctuation marks.

Here’s another example that doesn’t include any spaces or punctuation:

```Swift
let myString = "HelloSwift"
print(myString.count)
```

The output will be:

`10`

## Deep Dive:

While Swift uses the straightforward `count` property via Strings type, this has not been the case throughout the history of programming. Earlier languages like C required a function (`strlen()`) to determine the string length. C++ and Java make use of built-in methods like `.length()`. 

As an alternative, Swift also allows you to use the `utf16.count` or `unicodeScalars.count` based on what length you want to calculate. Differentiating between a Unicode scalar, a character, and a UTF-16 code unit can be important in internationalized scenarios.

The implementation of the string `count` property in Swift is interesting. It doesn't count characters one by one each time you call it. Swift strings are encoded in UTF-8 and might use one to four 8-bit bytes to store each character, but the string also stores the character counts for efficiency. 

## See Also:


2. [The Swift Programming Language](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html): More details about strings and characters in the official Swift language guide.