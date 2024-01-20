---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file in programming means extracting content from a file stored on your machine. This is vital in creating data-driven applications where information is required from other sources.

## How to:
The standard library in Kotlin makes reading a text file a very straightforward task. Here's a basic example showcasing it:

```Kotlin
import java.io.File

fun main() {
    val lines: List<String> = File("filename.txt").readLines()
    lines.forEach { println(it) }
}
```

This code reads the contents of a file named `filename.txt` line by line and prints each line. Update `filename.txt` with your own file path.

And your output should look something like this:

```
first line of text
second line of text
third line of text
```
This is a basic example but remember, in real-world scenarios you might be dealing with large files or might handle errors due to file unavailability or perform other checks.

## Deep Dive
If you're observing, Kotlin leverages Java's I/O operations under the hood. This stems from Kotlin's history as a language designed to operate on the JVM and be fully interoperable with Java. 

You can also use other methods to read a file such as `readText()`, which produces the whole content as a single string, or `bufferedReader().use { it.readText() }` for a more memory-friendly solution for larger files.

Note that all these methods throw an exception if the targeted file does not exist or is inaccessible. It's always wise to wrap your IO operations in a try-catch block or handle the exceptions suitably. 

## See Also
1. [Kotlin Standard Library Documentation - Reading a File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html)
2. [Kotlin Documentation - More on Exceptions](https://kotlinlang.org/docs/reference/exceptions.html)