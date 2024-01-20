---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:57:06.977235-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Kotlin lets you confirm whether a folder is present before you try to read or write files there. This helps avoid errors like `FileNotFoundException`, and helps your program make smart decisions, like creating the directory if it's missing.

## How to:

In Kotlin, you'll typically use `java.io.File` (from Java's standard library) to check for a directory. Here's a quick example:

```Kotlin
import java.io.File

fun main() {
    val directoryPath = "path/to/directory"
    val directory = File(directoryPath)

    if (directory.exists() && directory.isDirectory) {
        println("The directory exists!")
    } else {
        println("The directory does not exist.")
    }
}
```

Sample output when the directory exists:

```
The directory exists!
```

And when it doesn't:

```
The directory does not exist.
```

## Deep Dive

The `exists()` method in Java has been around since the early days of Java. When Kotlin came along, it kept strong interoperability with Java, allowing us to use Java libraries directly. `exists()` returns `true` if a file or directory exists, but to make sure it's a directory, not a file, we also check `isDirectory`.

Now, alternatives:

- **Kotlin's java.nio.file package**: It provides `Files.exists(path)` and `Files.isDirectory(path)`. These methods work similarly but offer more control over file attributes.

- **Kotlin-specific libraries**: Some community-driven libraries extend Kotlin's file-handling capabilities. They offer more idiomatic Kotlin solutions, but under the hood, they're often simply wrappers around Java's I/O classes.

Working with directories is a mix of checking presence (does it exist?) and type (is it a file or directory?). Both checks are key to preventing your program from tripping over unexpected filesystem states.

## See Also

- [`File` API documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html) - Dive into what you can do with `File`.
- [`Files` API documentation in java.nio.file package](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html) - For advanced file operations.
- [Kotlin's official documentation](https://kotlinlang.org/docs/home.html) - Learn more about Kotlin's capabilities.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/kotlin) - Find community-sourced issues and solutions.