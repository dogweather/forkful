---
title:                "Creating a temporary file"
html_title:           "Kotlin recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

If you are a developer working on an application that deals with large amounts of data or sensitive information, you may need to create temporary files. These files are used to store data temporarily and are automatically deleted after use, providing a more secure and efficient way of handling data.

## How To

Creating a temporary file in Kotlin is a simple process. First, we need to import the necessary classes for working with files:

```Kotlin
import java.io.File
import java.nio.file.Files
```

Next, we can use the `createTempFile()` function to create a temporary file with a prefix and suffix of our choice. For example, if we want to create a temporary file with a prefix "temp" and a suffix ".txt", we can use the following code:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
```

The `tempFile` variable now holds the reference to our newly created temporary file. We can then use the `writeText()` function to write data to this file:

```Kotlin
tempFile.writeText("This is some sample text.")
```

Finally, we can use the `delete()` function to delete the temporary file after it has served its purpose:

```Kotlin
tempFile.delete()
```

Sample Output:
```
temp1645125149939313978.txt
```

## Deep Dive

Behind the scenes, the `createTempFile()` function creates a unique file in the default temporary directory of the system. This file is then automatically deleted when the JVM terminates. Additionally, the `writeText()` and `delete()` functions are also atomic and thread-safe operations, ensuring the safety and reliability of our temporary file handling.

If we want to create a temporary file in a specific directory, we can use the `createTempFile()` function with two additional parameters specifying the directory path and a custom security manager:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt", File("custom/path/"), securityManager)
```

## See Also

- Kotlin File API: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Java IO File API: https://docs.oracle.com/javase/7/docs/api/java/io/File.html#delete()