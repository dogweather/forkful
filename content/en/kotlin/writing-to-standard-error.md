---
title:                "Writing to standard error"
html_title:           "Kotlin recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to standard error is a common practice in Kotlin programming for displaying error messages and debugging code. It allows developers to catch and handle errors in their code, making the program more robust and user-friendly.

## How To
To write to standard error in Kotlin, you can use the ```System.err.println()``` function. This function takes in a message as a parameter and prints it to the standard error stream.

```Kotlin
fun main() {
   // This message will be written to standard error
   System.err.println("An error has occurred.")
}
```

Running this code will produce the following output in the terminal:

```
An error has occurred.
```

Another way to write to standard error is by using the ```System.err.write()``` function. This function takes in a string as a parameter and writes it directly to the standard error stream.

```Kotlin
fun main() {
   // This string will be written to standard error
   System.err.write("This is a standard error message")
}
```

Running this code will produce the following output in the terminal:

```
This is a standard error message
```

## Deep Dive
In Kotlin, standard error is typically used for displaying error messages and logging information. It is different from standard output (using the ```System.out.println()``` function) which is used for regular program output.

By writing to standard error, developers can differentiate between normal program outputs and error messages, making it easier to identify and troubleshoot issues in the code. Additionally, using standard error also ensures that the error message is displayed even if the program crashes or throws an exception.

It is important to note that standard error output is not displayed on the user interface but is typically redirected to the terminal or a log file. This prevents any interruption or clutter in the program's user interface.

## See Also
- [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- [System class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system/index.html)
- [Error Handling in Kotlin](https://kotlinlang.org/docs/exceptions.html)