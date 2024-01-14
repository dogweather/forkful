---
title:                "Kotlin recipe: Writing to standard error"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why write to standard error?

As developers, we often use standard output to print information and debug our code. But what about standard error? Writing to standard error allows us to catch and handle any errors or exceptions that may occur during our program's execution. It's an essential tool for ensuring our code is robust and error-free.

## How To write to standard error in Kotlin

To write to standard error in Kotlin, we use the `System.err` stream. We can use the `println()` function to print a message to standard error, just like we do with standard output. Let's look at an example:

```Kotlin
fun main() {
    // Writing to standard error using println()
    System.err.println("Oops! Something went wrong.")
    
    // You can also use the print() function
    System.err.print("An error occurred. ")
    print("Check your code and try again.")
}
```

When we run this code, we get the following output:

```
Oops! Something went wrong.
An error occurred. Check your code and try again.
```

Notice how the messages printed with `System.err` are in red, indicating they were written to standard error. This allows us to easily distinguish them from messages printed to standard output.

## Deep Dive into writing to standard error

Behind the scenes, `System.err` redirects the output to the standard error stream of the operating system. This is typically the terminal or console where we run our code. This stream is useful for printing error messages that we want to see immediately, regardless of any output or logging that may be happening in our program.

One important thing to note is that messages written to standard error are not automatically flushed, meaning they may not appear immediately on our console. To ensure they are immediately visible, we can use the `flush()` function after writing to standard error.

```Kotlin
fun main() {
    // Use flush() to ensure immediate visibility of error messages
    System.err.println("This message will be immediately visible.")
    System.err.flush()
    
    // This message may not be visible until the program finishes
    System.err.println("This message may not be immediately visible.")
}
```

## See Also

- [Kotlin documentation on Standard Streams](https://kotlinlang.org/docs/standard-io.html#standard-streams)
- [Writing to standard error in Java](https://www.geeksforgeeks.org/writing-to-standard-error-in-java/)
- [Debugging and error handling in Kotlin](https://www.jetbrains.com/help/kotlin/debugging-and-error-handling.html)