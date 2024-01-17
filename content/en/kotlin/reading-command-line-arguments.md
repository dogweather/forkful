---
title:                "Reading command line arguments"
html_title:           "Kotlin recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the process of getting user-provided values or options when executing a Kotlin program from the command line. This allows developers to make their programs more flexible and user-friendly.

## How to:

To read command line arguments in Kotlin, we can use the `args` parameter available in the main function. This parameter is an array of strings that stores all the arguments passed from the command line.

Sample code:
```
fun main(args: Array<String>) {
    if (args.isNotEmpty()) { // checks if arguments were provided
        println("The arguments are: ${args.joinToString()}") // prints all arguments separated by a comma
    } else {
        println("No arguments provided.") // if no arguments, prints this statement
    }
}
```

Sample output when executing `kotlin Program.kt arg1 arg2 arg3`:
```
The arguments are: arg1, arg2, arg3
```

## Deep Dive:

1. Historical Context:
Command line arguments have been used since the early days of programming to provide input to a program without needing to constantly recompile the code. Initially, they were used for basic options such as file paths or program modes, but now they are used for a wide variety of purposes.

2. Alternatives:
Another way to get user input in Kotlin is by using the `readLine()` function, which allows users to input data directly in the terminal while the program is running. However, this method is not suitable for passing multiple arguments and is limited to basic data types.

3. Implementation Details:
In Kotlin, the `args` parameter is of type `Array<String>`, which means it can hold multiple string values. Therefore, we can access individual arguments using their index in the array, similar to accessing elements in a regular array.

## See Also:

- [Kotlin Docs for Command Line Arguments](https://kotlinlang.org/docs/command-line.html)
- [Stack Overflow Thread on Reading Command Line Arguments in Kotlin](https://stackoverflow.com/questions/27507719/kt-double-questionmark-and-star-sigil-notation-syntax)
- [GeeksforGeeks Tutorial on Command Line Arguments in Kotlin](https://www.geeksforgeeks.org/command-line-argument-in-kotlin/)