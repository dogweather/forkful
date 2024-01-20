---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# Swift String Concatenation: A Handy Guide
## What & Why?
String concatenation is the process of linking two or more strings to create a combined entity. Programmers do it to dynamically generate content, combine user input, or perform batch operations on strings.

## How to:
In Swift, there are several ways to concatenate strings. Here are the top methods:

1. **Plus Operator (+)**

```Swift
let str1 = "Hello"
let str2 = " World"
let message = str1 + str2
print(message)  // Outputs: Hello World
```

2. **Compound Assignment Operator (+=)**

```Swift
var str1 = "Hello"
let str2 = " World"
str1 += str2
print(str1)  // Outputs: Hello World
```

3. **Appending Method**

```Swift
var str1 = "Hello"
let str2 = " World"
str1.append(str2)
print(str1)  // Outputs: Hello World
```

## Deep Dive
Historically, string concatenation's importance was rooted in memory limitations and efficiency. Today, Swift optimizes its string concatenation operations, making it an easy and efficient process.

Alternatives to Swift's string concatenation include the use of Swift's `String Interpolation` which substitutes values into a string literal:

```Swift
let str1 = "Hello"
let str2 = " World"
let message = "\(str1)\(str2)"
print(message)  // Outputs: Hello World
```

While simple, Swift’s string concatenation subtly reveals a pattern in its design thought: String is a value type in Swift implying that every time we modify a string (like concatenating strings), we create a new string instance. This poses an upper limit on the performance you can squeeze out.

## See Also

More details can be found in Swift's official documentation:

1. [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Swift String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)