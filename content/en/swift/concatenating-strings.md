---
title:                "Swift recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

As a Swift programmer, you may have encountered the need to combine multiple string values into one. This process is known as concatenation and is a common task in programming. By concatenating strings, you can create new, dynamic messages, data structures, and more. It's a crucial skill to have in your toolbox as a programmer.

## How To

In Swift, there are several ways to concatenate strings. Let's take a look at some coding examples to understand the process better:

1. Using the `+` operator:

```Swift
let firstName = "John"
let lastName = "Smith"
let fullName = firstName + " " + lastName
print(fullName)
// Output: John Smith
```

2. Using the `+=` operator:

```Swift
var greeting = "Hello"
let name = "Emma"
greeting += ", " + name
print(greeting)
// Output: Hello, Emma
```

3. Using the `String(format:)` method:

```Swift
let age = 25
let message = String(format: "I am %d years old.", age)
print(message)
// Output: I am 25 years old.
```

4. Using string interpolation:

```Swift
let day = "Monday"
let weather = "sunny"
let forecast = "Today is \(day) and the weather is \(weather)."
print(forecast)
// Output: Today is Monday and the weather is sunny.
```

## Deep Dive

When concatenating strings, it's essential to consider the data types and the output you want to achieve. Swift provides different ways to combine strings, and each method has its advantages and disadvantages. 
For instance, using the `+` operator and string interpolation are swift and straightforward, but they can also become tedious and less efficient when dealing with multiple variables. On the other hand, using the `String(format:)` method allows for more flexibility and can handle different data types, but it can be more challenging to read and understand.

It's also crucial to keep in mind the difference between a string and a character when concatenating. Single quotes `' '` represent a character, while double quotes `" "` represent a string. So, if you want to concatenate a character with a string, you'll need to use the `String` initializer, like this:

```Swift
let initial = "T"
let lastName = "Swift"
let initials = String(initial) + lastName
print(initials)
// Output: TSwift
```

In addition, Swift has a built-in `join()` method that allows you to merge a sequence of strings with a separator. Here's an example:

```Swift
let fruits = ["apple", "banana", "orange"]
let fruitsString = fruits.joined(separator: ", ")
print(fruitsString)
// Output: apple, banana, orange
```

By using the `join()` method, you can easily concatenate multiple strings from an array.

## See Also

For more information on concatenating strings in Swift, check out the following resources:

- [Apple Official Documentation on Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hacking with Swift tutorial on concatenating strings](https://www.hackingwithswift.com/example-code/strings/how-to-join-strings-to-make-a-singlestring)
- [Ray Wenderlich tutorial on using string interpolation in Swift](https://www.raywenderlich.com/242-string-interpolation-in-swift-a-quick-guide)