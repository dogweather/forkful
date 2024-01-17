---
title:                "Reading a text file"
html_title:           "Kotlin recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file is the process of accessing and extracting information from a file that contains text, such as a .txt or .csv file. Programmers often do this in order to gather data or manipulate the contents of the file for further use in their code.

## How to:
```Kotlin
//To read a text file in Kotlin, you can use the readText() function from the standard library's File class.
val file = File("text_file.txt")
val text = file.readText()
//This will store the contents of the file in the variable "text" as a string.

//You can also use the readLines() function to read the file line by line and store each line in a list.
val file = File("text_file.txt")
val lines = file.readLines()
//This will store each line of the file in the variable "lines" as a list of strings.

//To read a specific line from the file, you can use the useLines() function and specify the line number you want to read.
val file = File("text_file.txt")
val line = file.useLines { it.elementAt(2) }
//This will store the third line of the file in the variable "line" as a string.

//To read a large file efficiently, you can use the forEachLine() function to read the file line by line without loading the entire file into memory.
val file = File("text_file.txt")
file.forEachLine {
    //write your code to manipulate the line here
}
//This will iterate through each line of the file, allowing you to process the data without overwhelming your system's memory.

```

## Deep Dive:
Reading text files has been a fundamental part of programming since the early days of computing. Before the widespread use of databases and cloud storage, text files were the primary means of storing and sharing data. While there are now alternative methods for storing and accessing data, text files still have a place in modern programming, especially for small and simple projects.

The File class used in Kotlin's standard library includes many useful functions for reading and writing files. In addition to the functions mentioned above, there are also functions for reading files as byte arrays, reading files from URLs, and more. With Kotlin's concise and intuitive syntax, reading text files is made even easier and more efficient.

## See Also:
- [Kotlin File class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java File class documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Stack Overflow: Reading a text file in Kotlin](https://stackoverflow.com/questions/52548610/read-text-file-into-string-array-using-kotlin/52548643)