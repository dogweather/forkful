---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments means picking up values passed to your program when it's started from the command line. It allows input to be provided without changing the program code, making it adaptable for different tasks.

## How to:

Acquiring command line arguments in Kotlin is straightforward. You do this via an array ('args') in the 'main' function. Here's a basic example:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

To run it, save the program in a file (say 'ArgsDemo.kt'), compile it (`kotlinc ArgsDemo.kt -include-runtime -d ArgsDemo.jar`), and run it with arguments (`java -jar ArgsDemo.jar firstArg secondArg`).

Output:

```
firstArg
secondArg
```

## Deep Dive

Command line arguments date back to the era where GUI was non-existent; it's the oldest way of parameterizing a program. Today, despite having much sophisticated options, it stands firm because of its simplicity.

Alternatively, you might interactively request data, read a file, or use a GUI form. Yet command line arguments remain in use, particularly in scripts or for testing. 

Kotlin’s implementation lays its elegance. The ‘args’ is an array of Strings. Each command line argument is a separate String. This keeps it clean, quick, and easy to comprehend.

## See Also

For more on Kotlin's command line programming, check its official [guide](https://kotlinlang.org/docs/tutorials/command-line.html). To deepen your knowledge about command line arguments, read this in-depth article [Command line arguments in Kotlin](https://zetcode.com/kotlin/cmdarguments/).