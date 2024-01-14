---
title:    "Kotlin recipe: Creating a temporary file"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming, especially when dealing with large amounts of data or when temporary storage is needed. These files are used to store information temporarily during the execution of a program and are typically deleted once the program has finished running. In Kotlin, creating temporary files is a simple and efficient process that can be useful in various scenarios.

## How To

To create a temporary file in Kotlin, we can use the `createTempFile()` function from the `kotlin.io` package. This function takes in a prefix for the file name, an optional suffix, and a parent directory (default is the temporary directory of the operating system) as parameters. Let's take a look at an example:

```Kotlin
val tempFile = createTempFile(prefix = "temp", suffix = ".txt")
println(tempFile.absolutePath)
```

Running this code would create a temporary file with the name "tempXXXXXX.txt" (where XXXXXX is a random string of characters) in the default temporary directory, and the absolute path to this file would be printed to the console. We can also specify the parent directory where we want the file to be created:

```Kotlin
val parentDir = File("/Users/username/temp/")
val tempFile = createTempFile(prefix = "temp", suffix = ".txt", directory = parentDir)
println(tempFile.absolutePath)
```

In this example, the file would be created in the specified directory instead of the default temporary directory. We can also specify only the prefix or suffix, or even omit both and let the function generate a random name for us.

After we have created the temporary file, we can use it like any other file. For example, we can write data to it using the `writeText()` function, and read data from it using the `readText()` function:

```Kotlin
tempFile.writeText("This is a temporary file.")
println(tempFile.readText())
```

The output would be: "This is a temporary file."

Once our program has finished running, the temporary file will be deleted automatically.

## Deep Dive

Under the hood, the `createTempFile()` function uses the `File.createTempFile()` method from the Java standard library, which in turn uses the `File.createTempFile()` method from the underlying operating system. This means that the behavior of temporary files may differ depending on the operating system your code is running on.

Additionally, the `createTempFile()` function returns a `File` object, which can be used to perform various operations on the temporary file, such as renaming or deleting it.

## See Also

If you're interested in learning more about temporary files in Kotlin, check out these resources:

- [Kotlin `createTempFile()` documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Java `createTempFile()` documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String,%20java.io.File))
- [Understanding Temporary Files in Java](https://www.baeldung.com/java-temporary-files)