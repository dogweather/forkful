---
title:                "Swift recipe: Concatenating strings"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Concatenating strings is a useful programming technique used to combine multiple strings into one. This comes in handy when you want to create dynamic messages, labels, or any type of text output in your program.

## How To
To begin concatenating strings in Swift, you first need to define two or more strings that you want to combine. This can be done using the `String` data type. For example, let's say we have two strings: 

```Swift
let firstName = "John"
let lastName = "Smith"
```

To concatenate these two strings, we simply use the `+` operator between them and assign the result to a new variable:

```Swift 
let fullName = firstName + lastName
```

This will result in a new string variable `fullName` with the value "JohnSmith". However, if we want to separate the first and last name with a space, we can do so by adding a space between the strings in the `+` operator:

```Swift
let fullName = firstName + " " + lastName
```

This will give us the desired output of "John Smith".

We can also use concatenation with variables and constants of other data types, such as integers or booleans:

```Swift
let age = 30
let message = "I am " + String(age) + " years old."
```

This will output "I am 30 years old." by converting the integer value of `age` into a string.

## Deep Dive
In Swift, there are multiple ways to concatenate strings besides using the `+` operator. Another option is to use string interpolation, which allows us to insert the value of a variable or constant directly into a string. This is done by placing a backslash and open/close parentheses around the variable/constant inside the string, like so:

```Swift
let message = "Hello, my name is \(fullName)."
```

This will result in the same output of "Hello, my name is John Smith."

Additionally, Swift has a `+=` operator specifically for string concatenation. This allows us to add a string to the end of another string. For example:

```Swift
var greeting = "Welcome "
greeting += name
```

This will output "Welcome John" if `name` is equal to "John".

## See Also
- [Concatenation (Computer Science) - Wikipedia](https://en.wikipedia.org/wiki/Concatenation_(computer_science))
- [String Interpolation in Swift](https://www.hackingwithswift.com/sixty/7/5/string-interpolation)
- [The Basics of String Concatenation in Swift](https://betterprogramming.pub/the-basics-of-string-concatenation-in-swift-5f8a84af1d1a)