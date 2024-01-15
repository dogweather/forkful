---
title:                "テキストファイルの書き方"
html_title:           "Kotlin: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a common and convenient way to store and manage data. By learning how to write a text file in Kotlin, you can easily manipulate and save your data in a structured format that can be easily accessed and modified.

## How To

Creating a text file in Kotlin is a simple process that involves a few basic steps:

1. Import the `java.io.File` class in your Kotlin file:
```Kotlin
import java.io.File
```

2. Create a `File` object with the desired file name and path:
```Kotlin
val file = File("myFile.txt")
```

3. Use the `bufferedWriter()` function to create a `BufferedWriter` object:
```Kotlin
val writer = file.bufferedWriter()
```

4. Use the `write()` function to add content to the file:
```Kotlin
writer.write("Hello World")
```

5. Use the `close()` function to save and close the file:
```Kotlin
writer.close()
```

This will create a new text file named "myFile.txt" in the same directory as your Kotlin file, with the content "Hello World" inside.

## Deep Dive

Kotlin's `java.io` package provides a variety of classes and functions for working with text files. The `File` class represents a file in the system, while the `BufferedWriter` class allows for efficient writing of large amounts of data to a file.

In addition to the `write()` function, the `BufferedWriter` class also has other useful functions such as `newLine()` to add a line break and `append()` to add content to an existing file.

It's important to use the `close()` function to properly save and close the file after writing to it. This ensures that all data is properly written and the file is not left open.

## See Also

- [Kotlin Documentation: java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin Documentation: java.io.BufferedWriter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-writer/)