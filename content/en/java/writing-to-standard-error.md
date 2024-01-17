---
title:                "Writing to standard error"
html_title:           "Java recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Printing to standard error is a way for Java programmers to display error messages or debug information in their code. It is a separate stream from standard output, which is typically used for normal program output. 

Programmers use writing to standard error in order to clearly differentiate error messages from regular output and to provide helpful information for troubleshooting and debugging purposes. 

## How to:

To write to standard error in Java, simply use the `System.err` stream and the `println` method. Here is an example code snippet:

```Java
System.err.println("Something went wrong!");
```

This will print the message "Something went wrong!" to the console in a different color, making it easier to spot and identify as an error message.

Additionally, you can also use the `e.printStackTrace()` method to print out more detailed information about a caught exception to the standard error stream. Here is another example:

```Java
try {
    // Some code that might throw an exception
} catch (Exception e) {
    e.printStackTrace();
}
```

This will print out the stack trace, including the line numbers of where the exception occurred, to the standard error stream.

## Deep Dive:

Writing to standard error has been a commonly used practice in programming, dating back to the early days of UNIX systems and the C programming language. It allows for better separation between regular output and error messages, making it easier to distinguish them and handle them appropriately.

An alternative to writing to standard error is to use logging frameworks, such as Log4j or SLF4J, which provide more advanced logging capabilities. However, for simple error messages and debugging purposes, using the `System.err` stream is a quick and easy solution.

When writing to standard error, it is important to note that the output will usually be displayed in a different color to indicate that it is an error message. This can be helpful in quickly identifying and troubleshooting issues in a program.

## See Also:

- [The official Java documentation on System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [A tutorial on standard output and error in Java](https://www.baeldung.com/java-printstream-printf)