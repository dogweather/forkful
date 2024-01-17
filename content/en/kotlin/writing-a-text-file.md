---
title:                "Writing a text file"
html_title:           "Kotlin recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file is the act of creating and saving a file that contains text-based data, using a computer program. Programmers often write text files to store persistent data or to generate reports.

## How to:
To write a text file in Kotlin, follow these three simple steps:
```
// 1. Import the necessary packages
import java.io.File
import java.io.FileWriter

// 2. Create a new file object, specifying the file name and location
val file = File("data.txt")

// 3. Write to the file using a FileWriter object
val writer = FileWriter(file)
writer.write("Hello World!")
writer.close()
```
The above code will create a file named "data.txt" in the same directory as your Kotlin file, and write the text "Hello World!" to it. You can also specify a different file path to save the file in a specific location.

## Deep Dive:
Text files have been used since the early days of computing to store data or communicate with other programs. They are simple and easily machine-readable, making them a popular choice among programmers. Alternatives to writing text files include using databases or creating binary files, but those options may be more complex or restrictive.

When writing a text file, it is important to consider character encoding to ensure compatibility with different operating systems and devices. In Kotlin, you can specify the encoding when creating a FileWriter object, such as "UTF-8" or "UTF-16".

## See Also:
- [Java File API documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Kotlin File API documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)