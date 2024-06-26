---
date: 2024-02-03 19:03:45.561841-07:00
description: "How to: In Kotlin, writing to stderr can be achieved using `System.err.println()`.\
  \ This method is similar to `System.out.println()` but directs the output\u2026"
lastmod: '2024-03-13T22:45:00.064132-06:00'
model: gpt-4-0125-preview
summary: In Kotlin, writing to stderr can be achieved using `System.err.println()`.
title: Writing to standard error
weight: 25
---

## How to:
In Kotlin, writing to stderr can be achieved using `System.err.println()`. This method is similar to `System.out.println()` but directs the output to the standard error stream rather than the standard output stream.

```kotlin
fun main() {
    System.err.println("This is an error message!")
}
```

Sample output:
```
This is an error message!
```

For more structured or complex applications, particularly those involving logging frameworks like Logback or SLF4J, you can configure loggers to write to stderr for certain log levels (e.g., ERROR).

Using SLF4J with Logback:

1. First, add the SLF4J API and Logback implementation to your `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Next, configure Logback (in `src/main/resources/logback.xml`) to direct error-level messages to stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Then, use SLF4J in your Kotlin code to log error messages:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("This is an error log message!")
}
```

Sample output (to stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - This is an error log message!
```
