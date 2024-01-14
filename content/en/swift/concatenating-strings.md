---
title:    "Swift recipe: Concatenating strings"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

One of the fundamental aspects of programming is working with strings. Strings are simply a collection of characters, such as letters, numbers, and symbols, that can be combined and manipulated to create meaningful information. In Swift, one way to manipulate strings is by concatenation, or the process of joining two or more strings together. This can be useful in creating dynamic and personalized text in your code, as well as organizing and formatting data.

## How To

Concatenating strings in Swift is a fairly simple process. First, you will need to define the strings you want to concatenate and assign them to variables:

```Swift
let greeting = "Hello"
let name = "John"
```

Next, you can use the `+` operator to combine the strings together:

```Swift
let message = greeting + " " + name
```

Note the use of the space between the two strings to ensure proper spacing in the final concatenated string. You can also directly combine strings and variables using the `+` operator:

```Swift
let age = 28
let introduction = "My name is " + name + " and I am " + String(age) + " years old."
```

In this example, we have also used the `String` initializer to convert the `age` variable from an integer to a string. Now, let's see the output of these concatenations:

```Swift
print(message)
// Output: "Hello John"

print(introduction)
// Output: "My name is John and I am 28 years old."
```

## Deep Dive

When it comes to concatenating strings, it's important to understand the different methods and options available to you. In Swift, you can use the `+=` operator to append a string to an existing string:

```Swift
var sentence = "I am learning "
sentence += "Swift."
// Outputs "I am learning Swift."
```

This can also be achieved using the `append()` method:

```Swift
var sentence = "I am learning "
sentence.append("Swift.")
// Outputs "I am learning Swift."
```

Additionally, you can use string interpolation, denoted by the `\(variable)` syntax, to insert variables directly into a string without the need for concatenation:

```Swift
let animal = "cat"
let sound = "meow"
let description = "The \(animal) makes a \(sound) sound."
// Outputs "The cat makes a meow sound."
```

Lastly, it's important to keep in mind the performance implications of concatenating strings. Each time a string is concatenated, a new string is created in memory. This can lead to decreased performance and memory usage if done excessively. To avoid this, Swift offers the `String(format:)` method which allows for efficient string formatting.

## See Also 
- Official Swift documentation on strings: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- More ways to manipulate strings in Swift: https://www.hackingwithswift.com/sixty/8/7/other-string-modifiers

With these concatenation techniques, you can now effectively manipulate strings in Swift and create dynamic and personalized text in your code. Happy coding!