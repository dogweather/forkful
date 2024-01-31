---
title:                "Logging"
date:                  2024-01-25T02:03:33.459956-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/logging.md"
---

{{< edit_this_page >}}

## What & Why?

Logging, at its core, is the practice of recording events and data from a software application to an external output, like a file or console. Programmers log stuff to trace through code, troubleshoot issues, and keep an eye on an app’s behavior in the wild, providing critical insights that can't be gleaned as effectively in any other way.

## How to:

In Kotlin, logging could be done using the built-in `println()` function for simple cases, or with more sophisticated libraries like SLF4J with Logback or Log4j for advanced needs.

Below is a basic example using `println()`:

```Kotlin
fun main() {
    println("Simple log message: Application started.")
    // ... some application logic here ...
    try {
        // Simulate an error
        throw Exception("Simulated error")
    } catch (e: Exception) {
        println("Error log message: " + e.message)
    }
}
```

Output:
```
Simple log message: Application started.
Error log message: Simulated error
```

And here's a snippet using SLF4J with Logback configured:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Structured log message: App launched.")
    // ... some application logic here ...
    try {
        // Simulate an error
        throw Exception("Simulated error")
    } catch (e: Exception) {
        logger.error("Structured error log: ", e)
    }
}
```

Assuming the appropriate Logback configuration, the output would be formatted and might look something like this when written to a log file:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Structured log message: App launched.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Structured error log: 
java.lang.Exception: Simulated error
   at com.myapp.Main.main(Main.kt:10)
```

## Deep Dive

Historically, logging in software developed along with the increasing complexity of applications and systems. Simple print statements were enough for the early days, where programs were often run and debugged by the developer themselves. But as systems networked and ran in different environments across different users, a robust and persistent logging system became crucial.

Before Kotlin became popular, Java developers widely adopted libraries like Log4j and later SLF4J. These have inspired similar practices in Kotlin, leveraging the interoperability of Kotlin with Java libraries. SLF4J acts as an abstraction layer, allowing the actual logging implementation to be swapped—usually Logback or Log4j2 are the preferred choices.

Kotlin also allows for multi-platform logging solutions that work across JVM, JavaScript, and Native, for instance, through the `expect`/`actual` mechanism, which abstracts away the platform-specific implementations.

In contrast to dedicated logging libraries, println persists as the simplest form of logging because it doesn't require additional setup or dependencies; however, it's usually unsuitable for production applications due to its lack of features like log levels, log rotation, and structured formats.

Other common features of advanced logging frameworks include:

- Log levels (DEBUG, INFO, WARN, ERROR, etc.) to categorize the urgency of log messages.
- Output to various sinks, like console, file, databases, or network services.
- Automatic log rotation and retention policies.
- Distributed tracing support for microservices architecture.
- Structured logging using formats like JSON, which integrates well with log analytics systems.

These tools and features are critical for maintaining a reliable, observable system especially in complex, distributed, or highly scaled environments.

## See Also

For further learning and insight into Kotlin logging, check out:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, the successor to Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Kotlin Multiplatform documentation on 'expect' and 'actual' declarations: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- A guide to structured logging in Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
