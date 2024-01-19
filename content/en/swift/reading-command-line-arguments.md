---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Reading Command Line Arguments in Swift

## What & Why?

Command line arguments are inputs received at the terminal when executing a Swift program. They're essential for providing flexibility to your program, enabling varied behavior based on user inputs.

## How to:

Reading command line arguments in Swift is straightforward. We use `CommandLine.arguments` which returns an array of String containing all sent arguments.

```Swift
let arguments = CommandLine.arguments
print("All arguments: \(arguments)")
```

If you run your program like `program firstArg secondArg`, the output will be:
`["./program", "firstArg", "secondArg"]`. The first argument is always the program's path.

Accessing individual arguments is simple array handling:

```Swift
let secondArgument = CommandLine.arguments[1]
print("Second argument: \(secondArgument)")
```

If you run `program firstArg secondArg`, the output will be: 
`Second argument: firstArg`.

## Deep Dive

Historically, languages like C used parameters in the main function to read command line arguments. This concept has been simplified in higher-level languages like Swift.

While `CommandLine.arguments` is a standard way of reading command line arguments in Swift, you could also use libraries like Commander or SwiftArgs to add more complex command-line parsing functionality.

Being aware of certain details like the first argument always being your program's path or that arguments are always strings (and potentially need parsing) can avoid common pitfalls in your command-line Swift programs.

## See Also

- Check [Apple’s Swift Documentation](https://developer.apple.com/documentation/swift/commandline) on `CommandLine.arguments` for more.
- Read about Swift libraries for command-line programs: [Commander](https://github.com/kylef/Commander) and [SwiftArgs](https://github.com/NSomar/SwiftArgs) for more complex needs.
- Visit [Swift Command-Line Programming](https://www.freecodecamp.org/news/get-started-with-the-basics-of-the-swift-command-line/) for an excellent guide on getting started.