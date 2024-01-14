---
title:                "Java recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Have you ever encountered an error in your Java program and wondered how to properly deal with it? Or do you want to improve the organization and debugging process of your code? Writing to standard error is a useful technique that can help you achieve these goals.

## How To
In Java, writing to standard error is accomplished by using the `System.err` output stream. Here's a simple code snippet that demonstrates this:

```Java
System.err.println("This is an error message");
```

This will print the message "This is an error message" to the standard error output. This output stream is separate from the standard output stream (accessed by `System.out`) and is typically used for error messages and other non-standard output.

To add more context to your error messages, you can use the `System.err.printf()` method, which allows you to format your error messages similar to how you would format strings with `String.format()`. Here's an example:

```Java
int num = 10;
System.err.printf("Error: The value of num is %d", num);
```

This will output "Error: The value of num is 10" to the standard error output.

## Deep Dive
Perhaps you're wondering why we can't just use `System.out` for error messages as well. This is because `System.out` is buffered, meaning that the messages will not be immediately displayed. On the other hand, `System.err` is not buffered, making it the ideal output stream for displaying error messages that need to be seen immediately.

Another advantage of writing to standard error is when dealing with multiple threads. Since `System.err` is not buffered, it can handle multiple threads trying to write to it at the same time without any issues.

Just like regular streams, you can also redirect standard error output by using `System.setErr()`. This can be helpful when you want to save all your error messages to a file for later analysis.

## See Also
- [Java System Class](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java PrintStream Class](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Java String.format() Method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)