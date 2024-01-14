---
title:                "Swift recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language. As developers, we often encounter unexpected errors or bugs in our code that can be difficult to identify. One of the ways to overcome this challenge is by using print statements to output valuable information about our code at specific points during execution.

## How To

In Swift, printing debug output is as simple as using the `print()` function. Let's say we have a variable `name` that stores a person's name. We can use the `print()` function to output the value of `name` to the console and see if it has been assigned the correct value.

```
var name = "John"

print("Name: \(name)")

```

The output in the console will be: `Name: John`

We can also print out multiple values in a single `print()` statement by separating them with commas. For example:

```
var age = 25

print("Name: \(name), Age: \(age)")

```

The output will be: `Name: John, Age: 25`

Another useful feature of the `print()` function is the ability to use string interpolation. We can insert variables, constants or expressions within a string using the `\()` syntax. This helps us to easily print out the values of these variables without having to constantly concatenate them with strings.

## Deep Dive

When printing debug output, it's important to keep in mind that too much information can also be overwhelming. We should carefully choose what to print and where to print it in order to effectively debug our code.

We can also include additional information in our print statements by using the `separator` and `terminator` parameters. The `separator` parameter allows us to specify a string that will be used to separate the different values being printed. By default, this is set to a single space. The `terminator` parameter allows us to specify what will be printed at the end of the statement. By default, this is set to a newline character. Here's an example:

```
print("Name:", name, "Age:", age, separator: " | ", terminator: " ")
print("Occupation: Developer")

```

The output will be: `Name: John | Age: 25 Occupation: Developer`

Additionally, we can also use the `debugPrint()` function instead of `print()` to provide more detailed information about our variables or expressions. This includes the type of the variable, its value, and a brief description if available. This can be useful when debugging complex data structures.

## See Also

To learn more about printing debug output in Swift, check out these resources:

- [Apple Developer Documentation on print() function](https://developer.apple.com/documentation/swift/1541053-print)
- [Swift by Sundell - Debugging in Swift](https://www.swiftbysundell.com/basics/debugging/)
- [Hacking with Swift - Debugging in Xcode](https://www.hackingwithswift.com/debugging)

Happy debugging!