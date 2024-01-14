---
title:    "Kotlin recipe: Creating a temporary file"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 

Creating temporary files is a common practice in programming, especially when working with large or sensitive data. Temporary files serve as a temporary storage location for data that is only needed for a short period of time, allowing for efficient data management and avoiding the clutter of permanent files. In this blog post, we will discuss how to create a temporary file using Kotlin.

## How To

To create a temporary file in Kotlin, we can use the `kotlin.io` library's `createTempFile()` function. This function takes in two parameters - the prefix and suffix of the temporary file name.

```Kotlin 
import java.io.File

fun main() {
    // Creating a temporary file with prefix "temp" and suffix ".txt"
    val tempFile = File.createTempFile("temp", ".txt")
    
    // Writing data to the temporary file
    tempFile.writeText("This is a temporary file.")
    
    // Reading data from the temporary file
    println("Temporary file content: ${tempFile.readText()}")
    
    // Deleting the temporary file
    tempFile.delete()
}
```

The above code will create a temporary file named "tempXXX.txt" (where XXX is a sequence of numbers) in the default temporary directory, which can be accessed through `File.createTempFile()` function's `parentFile` parameter. Here, the prefix "temp" is optional and can be omitted.

```
Temporary file content: This is a temporary file.
```

## Deep Dive

Behind the scenes, the `createTempFile()` function creates a unique temporary file in the default temporary directory, using the `File.createTempFile()` method from the `java.io` package. The prefix and suffix provided are used to generate a unique filename, but they do not necessarily appear in the actual name of the temporary file.

There are also alternative ways to create a temporary file in Kotlin, such as using the `TemporaryFile` class from the `io.temporal written` library or creating a `File` object with a temporary directory as its location.

## See Also

- [Kotlin createTempFile() function documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Java createTempFile() method documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Kotlin TemporaryFile library on GitHub](https://github.com/temporalio/written/blob/master/temporalio-utils/src/main/kotlin/io/temporal/written/TemporaryFile.kt)