---
title:                "Kotlin recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When writing code in Kotlin, it's important to understand how to effectively handle errors. Standard error, also known as STDERR, is an important tool for debugging and troubleshooting your code. In this blog post, we will explore why writing to standard error is crucial for any Kotlin programmer.

## How To

To write to standard error in Kotlin, we can use the `System.err.println()` function. This function takes in a string parameter and prints it to the standard error output. Let's take a look at an example:

```Kotlin
fun main() {
    val message = "This is an error message"
    System.err.println(message)
}
```

The output of this code will be:

```
This is an error message
```

As you can see, the message is printed to the standard error output instead of the standard output. This can be useful when troubleshooting code, as it allows us to differentiate between regular program output and error messages.

## Deep Dive

In Kotlin, there are three standard streams associated with a program: standard input (STDIN), standard output (STDOUT), and standard error (STDERR). STDERR is typically used for error messages and any output from the program that is not part of the desired result. By default, STDERR is displayed in red in the console, making it easier to distinguish from the regular output.

It's important to note that writing to standard error does not necessarily indicate that there is an error in the program. It simply means that the message is being sent to the standard error output. In fact, it is good practice to use STDERR for informative messages or warnings in order to provide more detailed information to the user of the program.

## See Also

- [Kotli