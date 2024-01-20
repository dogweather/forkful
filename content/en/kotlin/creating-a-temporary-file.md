---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creating Temporary Files with Kotlin

## What & Why?

Creating a temporary file is an act of making an impermanent storage area on disk for data. It's handy for staggered data processing, debugging, or when you're dealing with large chunks that can't fit in memory altogether.

## How to:

Creating a temporary file in Kotlin is straightforward, thanks to built-in function `createTempFile()` provided by `java.io.File` class. This function creates a new empty file in the default temporary-file directory.

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("tempFile", ".txt")
    println("Temporary file has been created.")
    println("Path : ${tempFile.absolutePath}")
}
```
When you run the above program, it creates a temporary file and prints its absolute path:

```Kotlin
Temporary file has been created.
Path: /tmp/tempFile1234567890.txt
```

## Deep Dive

Creating temporary files has an interesting history. It's been a common tactic since the early days of computing, used to bypass limitations in memory and storage.

In Kotlin, you aren't restricted to the system's default temporary directory. You can specify a different directory by passing it as a third argument to `createTempFile()`.

```Kotlin
val customDir = File("/custom/directory/path")
val tempFile = File.createTempFile("tempFile", ".txt", customDir)
```

It's worth mentioning that `createTempFile()` also accepts two string parameters, prefix and suffix, that become part of your file's name. This approach helps you easily identify and manage your temp files.

Alternatives to `createTempFile()`? You have the `Files.createTempFile()` from Java's NIO API. But the Kotlin `File` approach offers the same conciseness and makes code more Kotlin-idiomatic.

## See Also

1. `createTempDirectory()`: Another Kotlin method to create a temporary directory instead of a file.
2. Kotlin `Path` API: A more modern, flexible way to handle file paths.
3. [Kotlin Documentation](https://kotlinlang.org/docs/home.html): The best place to go for learning more about Kotlin.
4. [Java IO Tutorial](http://tutorials.jenkov.com/java-io/index.html): An in-depth look at Java's IO and NIO APIs.
5. [Temporary Files in Java](https://www.baeldung.com/java-create-temporary-file): A detailed article on creating temp files, if you want to work with Java's native functions.