---
title:                "Reading a text file"
date:                  2024-01-20T17:54:48.196059-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means pulling data from a file into your program, typically line by line. Programmers do this to process or analyze data that's stored externally.

## How to:

In Kotlin, you can read a text file easily using the `readLines()` function or the `useLines` block.

```Kotlin
import java.io.File

fun main() {
    // Read all lines at once
    val lines = File("example.txt").readLines()
    lines.forEach { line ->
        println(line)
    }

    // More efficient for large files
    File("example.txt").useLines { lines ->
        lines.forEach { line ->
            println(line)
        }
    }
}
```

Sample output (assuming `example.txt` contains two lines with "Hello" and "World"):

```
Hello
World
```

## Deep Dive

Historically, reading files in Java could be verbose and clunky. With Kotlin, the standard library provides handy extensions to make file reading simpler.

There are alternatives for file reading in Kotlin:
1. `readText()` reads the entire file content into a `String`.
2. `bufferedReader()` provides a `BufferedReader` that allows you to handle more complex use cases like reading huge files without consuming too much memory.

Implementation-wise, when you use `useLines`, it takes care of closing the file after execution, preventing potential memory leaks. It's a functional approach that's encouraged in Kotlin for managing resources effectively.

## See Also

- Kotlin documentation on reading files: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- `BufferedReader` documentation for more complex cases: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
