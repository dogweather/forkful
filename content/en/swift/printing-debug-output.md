---
title:                "Printing debug output"
html_title:           "Swift recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output refers to displaying information about a program's execution during development. This can be useful for debugging and understanding the flow of code. Programmers do this in order to identify and fix any errors or issues in their code.

## How to:

Printing debug output in Swift is simple and can be done using the `print()` function. This function takes in a variable or expression and displays its value in the debug console.

Example:

```Swift
let num = 5
print(num)

// Output: 5
```

You can also print multiple variables or expressions on the same line by separating them with a comma.

Example:

```Swift
let name = "John"
let age = 25
print("My name is \(name) and I am \(age) years old.")

// Output: My name is John and I am 25 years old.
```

Adding a custom string before the variable or expression can also be helpful for understanding the output.

Example:

```Swift
let sum = 10 + 5
print("The sum of 10 and 5 is \(sum).")

// Output: The sum of 10 and 5 is 15.
```

## Deep Dive:

Printing debug output has been a common practice in programming for a long time. Before the `print()` function, programmers used functions like `printf` in C and `cout` in C++ for similar purposes.

Apart from using the `print()` function, Swift also offers a rich debugging experience with the use of breakpoints and the debug console in Xcode. Breakpoints allow you to pause the code execution at a specific point and inspect the values of variables and expressions in that moment.

There are also alternative ways to output debug information, such as using the `NSLog` function in Swift, which displays the output in the console of the device. However, this should only be used for printing important messages as it can have a negative impact on performance.

## See Also:

- [Apple's Debugging Guide for Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html)
- [Using print() and NSLog() for debugging in Swift](https://fluffy.es/print-nslog-debugging-in-swift/)