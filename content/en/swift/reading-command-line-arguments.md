---
title:                "Swift recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential aspect of programming that can help optimize and enhance your code. Whether you are a beginner or an experienced developer, understanding how to read command line arguments can greatly benefit your Swift programming skills.

## How To

To read command line arguments in Swift, we first need to import the `Foundation` module to access the `Process` class. Then, we can create an instance of the `Process` class and use the `arguments` property to retrieve an array of string arguments passed in through the command line. Let's take a look at an example:

```
import Foundation

let arguments = ProcessInfo.processInfo.arguments

print("Command line arguments: \(arguments)")
```

Running this code in the command line with arguments, such as `swift main.swift argument1 argument2`, will output the following:

```
Command line arguments: ["/path/to/main.swift", "argument1", "argument2"]
```

Notice that the first argument in the array is always the path to the Swift file, while the remaining arguments are the ones passed in through the command line. We can also use `arguments.dropFirst()` to exclude the path to the file and only retrieve the user-provided arguments.

## Deep Dive

Command line arguments can also be used to pass in options or flags to our code. These can be specified by using a dash before the argument, such as `-v` for verbose mode or `-f` for a specific file.

We can check for the presence of these options by using the `contains` method on the `arguments` array. Let's take a look at an example:

```
import Foundation

let arguments = ProcessInfo.processInfo.arguments

if arguments.contains("-v") {
    print("Verbose mode activated")
}

if arguments.contains("-f") {
    let fileIndex = arguments.firstIndex(of: "-f")!

    print("File name: \(arguments[fileIndex + 1])")
}
```

In this example, we are checking for the presence of the `-v` and `-f` flags and retrieving additional information if they are present in the command line arguments. This allows us to create more dynamic and customizable code using command line input.

## See Also

- [Apple Developer Documentation on ProcessInfo](https://developer.apple.com/documentation/foundation/processinfo)
- [Swift Argument Parser Library](https://github.com/apple/swift-argument-parser)
- [Command Line Arguments Tutorial in Swift](https://medium.com/slalom-build/using-command-line-arguments-in-your-swift-script-29b5585ad847)