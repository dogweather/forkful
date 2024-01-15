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

## Why

Writing to standard error, also known as "stderr", can be a useful tool for Java programmers when debugging their code. It allows for error messages to be displayed separately from regular output, making it easier to identify and troubleshoot any issues in the code.

## How To

To write to stderr in Java, we can use the `System.err` object and the `print()` or `println()` methods. Here is a simple example:

```Java
System.err.println("This is an error message.");
```

The above code will print the message "This is an error message." in the console, but in red font instead of the standard black. This can be helpful in quickly identifying where an error has occurred in a long list of console output.

Additionally, we can also specify where the error message should appear by using the `setErr()` method on the `PrintStream` class. For example, if we want the error message to be displayed in a separate log file, we can do the following:

```Java
PrintStream errStream = new PrintStream(new File("error.log"));
System.setErr(errStream);
System.err.println("This will be printed to error.log");
```

## Deep Dive

Writing to stderr can be especially useful when dealing with exceptions in Java. By default, the stack trace for an exception is printed to stderr, making it easier to pinpoint the cause of the error. This can be further enhanced by using a logging framework such as Log4j, which allows for more detailed error messages to be displayed in stderr.

It's worth noting that writing to stderr should not be used for regular logging purposes. This should only be used for error messages and debugging purposes. For regular logging, it's best to use the `System.out` object and the `Logger` class.

## See Also

- [Java System Class JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Log4j Documentation](https://logging.apache.org/log4j/2.x/manual/api.html)

By following these simple steps, you can use writing to stderr as a helpful tool in your Java coding journey. Happy debugging!