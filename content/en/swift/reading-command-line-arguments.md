---
title:    "Swift recipe: Reading command line arguments"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why 
Command line arguments are an essential part of many programming languages, including Swift. They allow programs to receive input from users without the need for a graphical user interface. This means that developers can create powerful and efficient command-line tools, making it easier to automate tasks and handle large amounts of data.

## How To 
To read command line arguments in Swift, you can use the `CommandLine` class, which is part of the `Foundation` framework. 
Here is a simple example of how to read a single argument from the command line and print it to the console:

```Swift
let arguments = CommandLine.arguments
if arguments.count > 1 { // checking if at least one argument was provided
    let argument = arguments[1]
    print(argument)
}
```

Running this code with the following command: `swift CommandLineArguments.swift "Hello World!"` will result in the output `Hello World!`. 
Notice that the first argument in the `arguments` array is always the name of the executable file.

You can also use a `for-in` loop to iterate through all the arguments provided in the command line:
```Swift
for argument in arguments {
    print(argument)
}
```

If you need to convert the arguments to a specific data type, you can use the `Int()` or `Double()` initializer:
```Swift
let argument = arguments[1]
if let number = Int(argument) {
    print(number + 1) // adds 1 to the first argument provided
}
```

## Deep Dive 
There are a few important things to consider when working with command line arguments in Swift. Firstly, the `CommandLine` class is a singleton, meaning that there can only be one instance of it in your program. This ensures that all of your command line arguments are in one central location to avoid confusion.

Additionally, Swift automatically creates an array of strings for the command line arguments. This means that any user input, including numbers, will be represented as strings in your code. It's your responsibility to convert them to their appropriate data types if needed.

It's also worth noting that command line arguments are read-only, which means that you cannot modify them once they have been set.

## See Also
- [CommandLine class documentation](https://developer.apple.com/documentation/foundation/commandline)
- [Introduction to Swift command-line tools](https://www.swiftbysundell.com/posts/introduction-to-swift-command-line-tools)
- [Parsing Command Line Arguments in Swift](https://ashfurrow.com/blog/parsing-command-line-arguments-in-swift/)