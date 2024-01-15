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

## Why 

String concatenation is a fundamental skill that every Swift programmer should have in their arsenal. It allows you to combine multiple strings into one, making your code more efficient and easier to read. Whether you need to display a formatted string or manipulate user input, the ability to concatenate strings is crucial in creating dynamic and functional applications.

## How To 

One of the simplest ways to concatenate strings in Swift is by using the addition (+) operator. 

```Swift
let firstName = "John"
let lastName = "Doe"
let fullName = firstName + " " + lastName

print(fullName) // Output: John Doe
```

You can also use the String interpolation method, denoted by the backslash and parentheses (\()). This allows you to insert variables or expressions directly into a string. 

```Swift
let city = "New York"
let country = "USA"
let location = "I live in \(city), \(country)."

print(location) // Output: I live in New York, USA.
```

If you want to join multiple strings together, you can use the `joined(separator:)` method. This takes in a separator as a parameter and returns a single string with the original strings joined by the specified separator. 

```Swift
let fruits = ["apple", "orange", "banana"]
let joinedFruits = fruits.joined(separator: ", ")

print(joinedFruits) // Output: apple, orange, banana
```

Another useful method is `appending()` which adds a string to the end of another string. 

```Swift
var welcomeMessage = "Welcome"
welcomeMessage.append(" to my blog!")

print(welcomeMessage) // Output: Welcome to my blog!
```

## Deep Dive 

Behind the scenes, when you use the + operator or the `appending()` method, Swift is actually creating a new string containing the combined value. These methods are not mutating the original string, but rather creating a new one, which means you can use them on constants (using `let`) or variables (using `var`). 

String interpolation is also a form of string concatenation. It automatically creates a new string with the interpolated value. However, the original string and interpolated value must be in parentheses. 

```Swift
let number = 10
let message = "The number is \(number)" // Equivalent to "The number is " + String(number)

print(message) // Output: The number is 10
```

If you need to concatenate a large number of strings, it may be more efficient to use the `append()` method on a `String` instance instead of the `+` operator. This is because `append()` modifies the original string in-place, avoiding the creation of a new string each time. 

## See Also 

For more information on working with strings in Swift, check out the following resources:

- [The Swift Programming Language - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Concatenate Strings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-combine-strings-together)