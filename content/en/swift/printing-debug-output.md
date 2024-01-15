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

## Why

In programming, debugging is a crucial step to ensure that your code is functioning properly. One way to aid in the debugging process is by printing out specific information about the execution of your code. This can help you identify any errors or unexpected behavior.

## How To

To print debug output in Swift, we can use the `print()` function. Here is an example:

```Swift
var num1 = 10
var num2 = 5
print("The sum of \(num1) and \(num2) is \(num1+num2)")
```

The output of this code will be: `The sum of 10 and 5 is 15`. We can also print the values of variables by passing them into the `print()` function, like this:

```Swift
var name = "John"
print(name)
```

The output of this code will be: `John`.

You can also print multiple values on the same line by separating them with a comma, like this:

```Swift
var num1 = 10
var num2 = 5
print("The sum of", num1, "and", num2, "is", num1+num2)
```

The output of this code will be: `The sum of 10 and 5 is 15`.

## Deep Dive

The `print()` function has several optional parameters that allow us to customize the output. One of these parameters is `separator`, which allows us to specify the separator between the values being printed. By default, the separator is a single space, but we can change it to any character or string we want.

For example, if we want to print the values separated by a comma, we can do this:

```Swift
var num1 = 10
var num2 = 5
print(num1, num2, separator: ",")
```

The output of this code will be: `10,5`.

The `terminator` parameter allows us to specify the character or string to be printed after all values have been printed. By default, the terminator is a new line character, but we can change it to any character or string we want.

```Swift
var name = "John"
print(name, terminator: "")
print(" Doe")
```

The output of this code will be: `John Doe`.

## See Also

- [Apple's documentation on the `print()` function](https://developer.apple.com/documentation/swift/1541053-print)
- [Debugging in Swift](https://www.ralfebert.de/ios/tutorials/xcode-debugging/)