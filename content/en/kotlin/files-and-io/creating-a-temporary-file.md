---
date: 2024-01-20 17:40:42.355551-07:00
description: "Creating a temporary file is crafting a file that's meant to live short-term\
  \ on your filesystem, often for stuff like intermediary data. Programmers do it\u2026"
lastmod: '2024-03-13T22:45:00.066713-06:00'
model: gpt-4-1106-preview
summary: Creating a temporary file is crafting a file that's meant to live short-term
  on your filesystem, often for stuff like intermediary data.
title: Creating a temporary file
weight: 21
---

## How to:
Here's a quick way to make a temp file in Kotlin:

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("myTempFile", ".tmp")

    println("Temporary file created at: ${tempFile.absolutePath}")

    // Write to temp file
    tempFile.writeText("Kotlin is pretty neat, huh?")

    // Delete on exit
    tempFile.deleteOnExit()
}
```

Output will be something like:

```
Temporary file created at: /tmp/myTempFile1234567890.tmp
```

Your temp file path will differ. It'll have a unique name so don't sweat over naming clashes.

## Deep Dive
The `File.createTempFile()` method is golden for ad-hoc file generation. It's been around since Java's early days and Kotlin, being a JVM language, takes full advantage.

Some alternatives:
- `Files.createTempFile()` from `java.nio.file` offers more control, like setting file attributes.
- In-memory databases or caches could replace temp files for some use-cases (like `H2` or `Redis`).

By default, temp files are stored in the system's default temporary file directory, but you can specify your own path. Remember to clean up after yourself; temp files aren't guaranteed to be deleted after your program runs. The `deleteOnExit()` method ensures the file is deleted when the JVM shuts down, but it's not fail-safe for long-running apps.

## See Also
More on temp files in Kotlin and Java:
- Kotlin's official `File` documentation: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Java's `File` class: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- For a deeper understanding of file attributes: [https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html](https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html)
