---
title:                "Swift recipe: Reading command line arguments"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

When writing a Swift program, you may encounter situations where you need to get information from the user directly in the command line. This is where reading command line arguments becomes useful. It allows you to access and use input from the user without the need for a graphical user interface.

## How To

Reading command line arguments in Swift is a straightforward process. First, you need to create a variable to store the arguments. This is usually an array of strings as each argument will be stored as a separate element. You can do this by using the `CommandLine.arguments` property.

```Swift
let args = CommandLine.arguments
```

To access a specific argument, you can use the bracket notation and specify the index of the argument you want to access. Remember, the first argument (index 0) will always be the name of the executable file.

```Swift
let firstArg = args[1] // accessing the second argument
```

To print out the arguments, you can use a simple `for` loop and print each element in the `args` array.

```Swift
for arg in args {
  print(arg)
}
```

Now, let's say you have a simple program that adds two numbers together. You can read the numbers from the command line and perform the addition as follows:

```Swift
let firstNum = Int(args[1]) ?? 0 // converting argument to an integer
let secondNum = Int(args[2]) ?? 0 // converting argument to an integer
let result = firstNum + secondNum
print("The sum of the two numbers is \(result).")
```

Running the program in the command line with the arguments `3 7` would produce the following output:

```
The sum of the two numbers is 10.
```

## Deep Dive

There are a few things to keep in mind when reading command line arguments in Swift. First, remember to always check for the number of arguments before accessing them. If there are not enough arguments passed in, your program may crash. Additionally, you may need to do some data type conversion depending on what type of input you are expecting.

It's also worth noting that the order in which the arguments are passed in the command line matters. For example, if your program expects a first name and a last name, but the user passes them in the reverse order, the output will also be reversed.

Lastly, you can also use flags or options as arguments in Swift by using the `CommandLine.option` property. This can be particularly useful in more complex programs that require different behaviors based on user input.

## See Also

- [Official Swift Language Guide](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)
- [Working with Command Line Arguments in Swift](https://www.hackingwithswift.com/articles/162/how-to-use-command-line-arguments-in-swift)