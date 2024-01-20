---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the act of sending text data or messages from the running code to a console. It helps programmers identify and fix bugs by providing a snapshot of how variables and conditions change state during the program execution.

## How To:

Printing debug output in Java is straightforward. You can use `System.out.println()` or `System.err.println()`. Here's a simple example:

```Java
public class DebugExample {
    public static void main(String[] args) {
        String name = "John Doe";
        int age = 45;
        
        // Debug output
        System.out.println("Debug info: Name is " + name + ", Age is " + age);
    }
}
```
When you run this code, you see the following output on your console.

```Java
Debug info: Name is John Doe, Age is 45
```
In this way, you can print debug outputs to trace your code's execution in Java.

## Deep Dive

Historically, debug output was used in various programming languages to track and fix bugs during the development phase. In Java, `System.out.println()` acts as a simple tool to trace your code. However, this approach can become cumbersome with larger codebases. 

Alternatives include using Java's built-in logging API (`java.util.logging`) or third-party libraries like Apache Log4j, SLF4J, or Logback. These provide better control over where your log messages go, how they look, and under what conditions they're printed.

A significant difference exists between `System.out.println()` and `System.err.println()` in the way they handle output. The former routes to "standard output" (usually a console) and is buffered. That means the output may not immediately appear on the console. On the other hand, `System.err.println()` is unbuffered and routes to "standard error", making the output immediately visible. 

## See Also

For further reading, consider these sources:

- [Oracle’s Guide on Debugging](https://docs.oracle.com/en/java/javase/14/troubleshoot/debugging.html)
- [Introduction to the Java Logging API](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Apache Log4j Manual](https://logging.apache.org/log4j/2.x/manual/index.html)