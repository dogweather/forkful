---
title:    "Kotlin recipe: Checking if a directory exists"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
When working with files and directories, it's important to first check if a directory exists before attempting to perform any operations on it. This ensures that your code runs smoothly and avoids any potential errors.

## How To
To check if a directory exists in Kotlin, we can use the `exists()` method from the `java.io.File` class. This method returns a Boolean value, `true` if the directory exists and `false` if it doesn't.

Let's take a look at an example:

```Kotlin
// Import the java.io package
import java.io.*

// Create a File object for the directory we want to check
val directory = File("/home/user/documents")

// Call the exists() method on the File object
println(directory.exists())

// Output: true
```

In this example, we import the `java.io` package and create a `File` object for the directory we want to check, which in this case is `/home/user/documents`. Then, we call the `exists()` method on the `directory` object and print the result, which is `true` since the directory does exist.

We can also use the `exists()` method to check if a file exists. Here's an example:

```Kotlin
// Create a File object for the file we want to check
val file = File("/home/user/documents/sample.txt")

// Call the exists() method on the File object
println(file.exists())

// Output: false
```

In this example, we have a `File` object for a file that does not exist, so the `exists()` method returns `false`.

## Deep Dive
Under the hood, the `exists()` method makes use of the `java.io.FileSystem` class which contains platform dependent methods for manipulating files and directories. It uses these methods to check for the existence of the specified directory or file.

Additionally, it's important to note that the `exists()` method is just a simple check for existence and does not necessarily guarantee that the directory or file is accessible or readable. Other factors such as permissions and file locking could still prevent file operations from being successful.

## See Also
- [Official Kotlin Documentation on File Handling](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Kotlin File Operations Tutorial](https://www.baeldung.com/kotlin-file-operations)

By using the `exists()` method, we can ensure that our code is running efficiently and avoid any potential errors when working with files and directories. Try it out in your next Kotlin project!