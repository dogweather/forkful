---
title:    "Swift recipe: Reading command line arguments"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

Are you tired of manually inputting data every time you run your Swift program? Want to make your code more efficient and interactive? Then keep reading to learn about the power of command line arguments.

## How To 

```Swift
import Foundation

// Accessing command line arguments
let arguments = CommandLine.arguments
print("Number of arguments: \(arguments.count)")
print("Arguments: \(arguments)")

// Accessing specific arguments
let firstArgument = arguments[1]
print("First argument: \(firstArgument)")

// Converting arguments to specific data types
let numberArgument = Int(arguments[2]) ?? 0
print("Second argument multiplied by 2: \(numberArgument * 2)")
```

Sample output:
```
Number of arguments: 3
Arguments: ["program_name", "hello", "10"]
First argument: hello
Second argument multiplied by 2: 20
```

## Deep Dive

Command line arguments allow you to pass data into your Swift program from the terminal. This can be useful when you want to make your code more dynamic and customizable for different inputs. It also streamlines the process of testing your code by eliminating the need to constantly change data within your code.

To access command line arguments in Swift, we use the `CommandLine.arguments` property. This returns an array of strings, with the first argument being the name of the program itself. To access specific arguments, we can use array indexing (remember, arrays are zero-indexed so the first argument is at index 0).

By default, command line arguments are read in as strings. However, we can convert them to different data types using Swift's built-in type conversion methods. For example, we can use `Int()` to convert a string argument to an integer. It's important to note that the conversion may fail if the argument is not of the expected data type, so we use the nil coalescing operator `??` to provide a default value in case of failure.

Now that you know how to access and convert command line arguments, you can use them in your Swift programs to make them more dynamic and interactive.

## See Also 

- [Swift Command Line Arguments](https://www.swiftbysundell.com/basics/command-line-arguments/)
- [Swift Programming Language Guide: Type Casting](https://docs.swift.org/swift-book/LanguageGuide/TypeCasting.html)
- [Introduction to Swift: Optionals](https://www.hackingwithswift.com/sixty/8/5/introducing-optionals)