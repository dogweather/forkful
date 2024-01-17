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

## What & Why?
Checking if a directory exists is a common task that programmers use to determine whether a specific directory exists in a given path or not. It is important because it allows the program to make decisions based on whether the directory is present or not.

## How to:
To check if a directory exists in Kotlin, we use the File class from the java.io package. Here's an example of how we can check if a directory named "myDirectory" exists in the current directory:
```Kotlin
import java.io.File

val directory = File("myDirectory")
if (directory.exists()) {
    println("Directory exists!")
} else {
    println("Directory does not exist.")
}
```
Output:
```
Directory exists!
```
In the above example, we first create a File object for the directory we want to check. Then, we use the exists() method to check if the directory exists or not. If it exists, we print a message to indicate that, otherwise, we print a different message.

## Deep Dive:
Checking if a directory exists has been a common task for a long time, even before the advent of programming languages like Kotlin. It is necessary, especially when working with files and directories, to avoid errors or unexpected behavior in our programs.

An alternative to using the File class in Kotlin is to use the java.nio.file.Files class and its exists() method. This class provides more options, such as checking for symbolic links or hidden files.

When checking for the existence of a directory, it is also important to consider file permissions, as it can affect the results of the check. For example, if a program does not have the permission to access a specific directory, it will always return false when checking for its existence.

## See Also:
- Official documentation for the File class in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Java documentation for the File class: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Official documentation for the Files class in Java: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html