---
title:                "Checking if a directory exists"
html_title:           "Kotlin recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an important aspect of programming as it ensures the smooth functioning of file operations. By verifying the existence of a directory, you can avoid errors and ensure that your code runs smoothly.

## How To

```Kotlin
val directory = File("/path/to/directory") // replace with the desired directory path
if (directory.exists()) { // checks if the directory exists
    println("Directory exists")
} else {
    println("Directory does not exist")
}
```

## Deep Dive

In Kotlin, the `File` class provides the `exists()` method that returns a boolean value indicating the existence of a file or directory with the specified path. This method can also be used to check the existence of a file.

Apart from `exists()`, the `File` class also provides other methods such as `isDirectory()` and `isFile()` that can be used to determine if the given path refers to a directory or a file.

It is important to note that checking for the existence of a directory does not mean that the program has permission to read or write to that directory. To perform read or write operations, additional checks and permissions may be required.

## See Also

- [Kotlin File Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/kotlin.-file/index.html)
- [Kotlin Check if File or Directory Exists](https://www.baeldung.com/kotlin-check-if-file-directory-exists)