---
title:                "Swift recipe: Printing debug output"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language, and printing debug output is a powerful tool that can assist in the process. By printing messages or values during runtime, you can get a better understanding of what your code is doing and how it is functioning. This can be helpful in detecting and fixing errors or simply gaining a better understanding of the flow of your program.

## How To

Printing debug output in Swift is a straightforward process. You can use the built-in `print()` function to output any information you want to the console. Let's take a look at some examples:

```Swift
// Printing a message
print("Hello, world!")

// Printing a variable
let age = 25
print("User's age is: \(age)")

// Printing a boolean value
let isStudent = true
print("Is the user a student? \(isStudent)")
```

The output for these examples would be:

```
Hello, world!
User's age is: 25
Is the user a student? true
```

You can also print multiple values at once, by separating them with a comma in the `print()` function:

```Swift
let name = "Sara"
let gender = "female"
print("User's name is: \(name) and their gender is: \(gender)")
```

The output for this example would be:

```
User's name is: Sara and their gender is: female
```

## Deep Dive

There are a few more things to keep in mind when printing debug output in Swift. Firstly, you can use the `separator` and `terminator` parameters in the `print()` function to customize the output. The `separator` parameter allows you to specify what character or string should be used to separate the different values, and the `terminator` parameter lets you specify what should be added at the end of the output. By default, the `separator` is a space and the `terminator` is a new line.

```Swift
let score = 85
print("User's score is: ", score, separator: "-", terminator: "!")
```

The output for this example would be:

```
User's score is: 85!
```

Another useful tip is to use the `debugPrint()` function instead of `print()`. This will print the values in a format that is more suitable for debugging purposes, making it easier to read and understand.

Additionally, you can add conditions to when the debug output should be printed, using the `#if` compiler directive. This can be helpful when you only want to print output in a specific environment, such as during development or testing.

## See Also

For more information about printing debug output in Swift, check out the official documentation:

- [Debugging in Swift](https://developer.apple.com/swift/blog/?id=16)
- [Print function](https://developer.apple.com/documentation/swift/1541053-print)
- [Debug Print function](https://developer.apple.com/documentation/swift/special_topics/advanced_operators/compound_assignment_operators)

Happy debugging!