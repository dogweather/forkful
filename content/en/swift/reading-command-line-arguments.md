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

## What & Why?

Reading command line arguments is the process of allowing a program to take input from the user through the command line interface. Programmers do this to make their programs more interactive and flexible, as command line arguments allow for customization and specific input from the user.

## How to:

To read command line arguments in Swift, we can use the `CommandLine` class. Here's an example of how to access the arguments and print them out:

```Swift
// Assuming we run the program with two arguments: "hello" and "world"
let arguments = CommandLine.arguments

// arguments[0] will be the path to the program itself
// arguments[1] will be "hello"
// arguments[2] will be "world"
print("Hello \(arguments[1]) \(arguments[2])!") // Output: Hello hello world!
```

## Deep Dive

Command line arguments have been around since the early days of computing, where programs were run through terminals. They allow for user input without the need for a graphical user interface. Alternatives to command line arguments include using a user interface or input files.

In Swift, reading command line arguments is made easy through the `CommandLine` class. It provides access to not only the arguments themselves, but also information about the environment and the current working directory. Command line arguments can also be easily validated and checked for errors.

## See Also

- [Apple's documentation on CommandLine](https://developer.apple.com/documentation/swift/commandline)
- [Article on command line arguments in C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Alternative to command line arguments: GUI inputs](https://www.techopedia.com/definition/253/self-service-user-interface)