---
title:                "Reading command line arguments"
html_title:           "Swift recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
So you want to learn how to read command line arguments in Swift? Well, lucky for you, it's a useful skill to have when working on command line tools or scripts. It allows you to pass information to your code before it even starts running.

## How To
```Swift
import Foundation

let cliArgs = CommandLine.arguments

print("Number of arguments passed: \(cliArgs.count)")

for (index, argument) in cliArgs.enumerated() {
    print("Argument #\(index+1): \(argument)")
}
```
The `CommandLine` class in Swift gives us access to the arguments passed when executing a command line tool or script. We first import the `Foundation` framework which is necessary to use the class. Then, we use the `CommandLine.arguments` property to access an array of `String` values, representing each command line argument. We can then manipulate and use these arguments in our code.

Running the above code with the command line arguments `SwiftCLI arg1 arg2 arg3` would output:
```
Number of arguments passed: 4
Argument #1: SwiftCLI
Argument #2: arg1
Argument #3: arg2
Argument #4: arg3
```

## Deep Dive
You may have noticed that the `cliArgs` array contains the program's name as the first element. This is useful to keep in mind when accessing specific arguments. Additionally, the `CommandLine` class provides a `namedArguments` property for accessing named arguments in the format of `--name=value`.

You may also want to validate the number and type of command line arguments before using them in your code. The `CommandLine` class also has a `validateArguments` method that allows you to specify the number of required arguments and their expected data types.

## See Also
- [Apple Documentation on CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Swift Command Line Arguments: Step by Step Tutorial](https://www.agnosticdev.com/blog-entry/swift-command-line-arguments-step-step-tutorial)