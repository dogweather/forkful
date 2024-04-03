---
date: 2024-01-25 02:03:39.164834-07:00
description: "Logging is essentially the process of recording events that occur within\
  \ a software application. Programmers log these events to capture runtime\u2026"
lastmod: '2024-03-13T22:44:59.978094-06:00'
model: gpt-4-1106-preview
summary: Logging is essentially the process of recording events that occur within
  a software application.
title: Logging
weight: 17
---

## How to:
Here's a simple way to get started with logging in Java using the built-in `java.util.logging` package.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging an INFO-level message");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Exception occur", e);
        }
    }
}
```

This would produce output along the lines of:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logging an INFO-level message
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Exception occur
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Deep Dive
Logging in Java has evolved quite a bit. Historically, logging was more ad-hoc with system outputs and self-written mechanisms. However, the need for standardization led to logging APIs like `Log4j` and `SLF4J`. The `java.util.logging` package itself was introduced in JDK 1.4, providing a standardized way to log messages.

Alternatives to `java.util.logging` (JUL) include Log4j 2 and SLF4J. While JUL is built into Java and thus doesn't require additional dependencies, both Log4j 2 and SLF4J offer more advanced features like more granular control over logging configuration, asynchronous logging, and better performance.

Implementation-wise, logging can either be synchronous, where each log message is processed in the thread that generated it, or asynchronous, where messages are handed off to a separate thread. Asynchronous logging can improve performance but introduces complexity as one must handle concurrency and ensure that log messages are not lost on application crash.

## See Also
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracle's official logging overview](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial on java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
