---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) is a stream where a program writes its error messages. Programmers use it to separate error logs from standard output (stdout) to debug more efficiently and streamline logging.

## How to:

Here's a simple Kotlin snippet to print to standard error:

```kotlin
fun main() {
    System.err.println("Oops, an error occurred.")
}
```

And the output in your console will look like this (style may vary by terminal):

```
Oops, an error occurred.
```

## Deep Dive

Originally in Unix-like systems, the rationale for stderr is clear-cut: stderr allows error messages to be sent to the screen or another file than normal output. It helps in distinguishing normal data from error messages, especially useful when output is piped elsewhere.

Alternatives to `System.err.println` include using a logging framework like Logback or log4j, which offers more control and options like logging levels and file output.

The `System.err` in Kotlin is inherited from Java's `System` class, similar to `System.out` for standard output, both are PrintStream objects. By default, `System.err` prints to the console. However, it can be redirected to write to a file or a different output stream.

## See Also

- The Kotlin documentation on basic I/O: https://kotlinlang.org/docs/basic-io.html
- Information about Unix standard streams: https://en.wikipedia.org/wiki/Standard_streams
- Logback, a popular logging framework: http://logback.qos.ch/
- Apache log4j, another logging framework: https://logging.apache.org/log4j/2.x/
