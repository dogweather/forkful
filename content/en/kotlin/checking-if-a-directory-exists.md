---
title:                "Kotlin recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When writing a Kotlin program, it is important to ensure that the program is able to handle all possible scenarios and inputs to avoid errors and crashes. One important scenario to consider is the existence of a directory that the program will be using. This is especially important if the program is performing file operations, as attempting to access a non-existent directory can cause the program to crash.

## How To

To check if a directory exists in Kotlin, we can use the ```exists()``` method from the ```java.io.File``` class. Let's take a look at an example:

```Kotlin
import java.io.File

val directory = File("directory_path")

if (directory.exists()) {
    println("The directory exists!")
} else {
    println("The directory does not exist.")
}
```

In this example, we first create a ```File``` object using the path to the directory we want to check. Then, we use the ```exists()``` method to check if the directory exists. If it does, we print a message confirming its existence. Otherwise, we print a message stating that the directory does not exist.

We can also check if a directory exists by using the ```isDirectory()``` method, which returns a boolean value indicating whether the file is a directory or not. Let's see this in action:

```Kotlin
import java.io.File

val directory = File("directory_path")

if (directory.isDirectory()) {
    println("The path is a directory.")
} else {
    println("The path is not a directory.")
}
```

## Deep Dive

Under the hood, the ```exists()``` and ```isDirectory()``` methods use the ```java.io.File``` class, which provides a simple interface for working with files and directories in Java and Kotlin. The ```File``` class also provides methods for creating and deleting files and directories, as well as checking for other file attributes such as readability and writability.

It is worth noting that the ```exists()``` and ```isDirectory()``` methods do not guarantee the existence or non-existence of a directory. This is because the directory may have been created or deleted by another process after the check was performed. Therefore, it is important to handle any possible exceptions that may occur if the directory does not exist, and to always double check for its existence before performing any file operations.

## See Also

- [Kotlin Documentation: java.io.File class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Checking if a File or Directory exists in Kotlin](https://www.baeldung.com/kotlin/check-file-directory-exists)