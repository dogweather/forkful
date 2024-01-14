---
title:    "Kotlin recipe: Checking if a directory exists"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why
In Kotlin, checking if a directory exists is a common task when working with files and directories. This can be useful in a variety of scenarios, such as checking the existence of a directory before creating a new one, or making sure that a certain directory path is valid before attempting to access it.

## How To
To check if a directory exists in Kotlin, we can use the `exists()` function from the `java.io.File` class. This function returns a boolean value, `true` if the directory exists and `false` if it does not.

```Kotlin
val directory = File("~/Documents/blog-posts")
if (directory.exists()) {
    println("The directory exists!")
} else {
    println("The directory does not exist.")
}

// Output: The directory exists!
```

We can also use the `isDirectory()` function to check if the file path refers to a directory. This can be useful if we want to make sure that the path we are checking is specifically a directory and not a regular file.

```Kotlin
val filePath = File("~/Downloads/prices.csv")
if (filePath.exists()) {
    if (filePath.isDirectory()) {
        println("The path is a directory.")
    } else {
        println("The path is not a directory.")
    }
} else {
    println("The path does not exist.")
}

// Output: The path is not a directory.
```

## Deep Dive
In Kotlin, the `exists()` function is actually inherited from the `java.io.File` class, which means it is available not just for directories but also for regular files. This can be a bit confusing, so it is important to also use the `isDirectory()` function to differentiate between the two.

We can also use `listFiles()` function to get an array of `File` objects representing the contents of the directory. This can be useful if we want to further manipulate the files and directories within the specified directory.

```Kotlin
val directory = File("~/Documents/blog-posts")
if (directory.exists() && directory.isDirectory()) {
    val directoryContents = directory.listFiles()
    println("The contents of the directory are: ${directoryContents.joinToString(", ") { it.name }}")
} else {
    println("The directory does not exist or is not a directory.")
}

// Output: The contents of the directory are: blog-post1, blog-post2, blog-post3
```

## See Also
- Official Kotlin documentation for `java.io.File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Tutorial on working with files and directories in Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_working_with_files.htm