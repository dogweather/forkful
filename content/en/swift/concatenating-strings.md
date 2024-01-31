---
title:                "Concatenating strings"
date:                  2024-01-20T17:35:33.715662-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is gluing separate strings together to make a new one. Programmers do it to combine text dynamically, like putting together greetings, messages, or just to structure data in a readable format.

## How to:
```Swift
let firstName = "Taylor"
let lastName = "Swift"
let fullName = firstName + " " + lastName  // Using the + operator
print(fullName)  // Output: "Taylor Swift"

let age = 31
let greeting = "Hello, \(firstName)! You're \(age) years old."  // Using string interpolation
print(greeting)  // Output: "Hello, Taylor! You're 31 years old."

var message = "This"
message += " is" // Using += operator to append to a string
message += " Sparta!"
print(message)  // Output: "This is Sparta!"
```

## Deep Dive
Way back, folks in programming languages like C had to manually shuffle strings around with functions, dealing with arrays and null-terminated strings. Swift makes it easy. The '+' operator for strings came from languages like Java and C++, bringing over the familiar way to snap strings together.

There are options beyond '+'. String interpolation in Swift is more than just about being fancy – it's a type-safe way to embed values directly within your string. No need to cast types or worry you'll mismatch something.

Advanced concatenation involves more than just slinging words around. When performance is key, using '+=' recklessly can slow you down. Why? Because if you're adding to a string in a loop, Swift may create new strings each time, which isn't so snappy. Instead, consider using 'join()' or 'append()' of 'String' for efficiency, especially with big data or complex loops.

```Swift
// Efficient concatenation with `join()`
let words = ["Once", "upon", "a", "time"]
let story = words.joined(separator: " ")  // Efficient for joining array elements
print(story)  // Output: "Once upon a time"

// Using 'append(contentsOf:)' for appending substrings
var quote = "I think, "
quote.append(contentsOf: "therefore I am")
print(quote)  // Output: "I think, therefore I am"
```

## See Also
- Swift Documentation on Strings: [Swift.org Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Apple's String Programming Guide: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
