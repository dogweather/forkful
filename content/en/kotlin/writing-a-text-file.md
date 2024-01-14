---
title:                "Kotlin recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you may find yourself needing to write and read data from files frequently. One common type of file is a text file, which contains plain text that can be easily read and modified by humans. In this blog post, we will explore why writing a text file is a useful skill to have in your programming arsenal.

## How To
Writing a text file in Kotlin is a simple process. First, we will need to create a FileWriter object, passing in the name of the file we want to write to as a parameter. We can also specify if we want to append to an existing file or create a new one if it doesn't exist. 

```
val fileWriter = FileWriter("myFile.txt", true)
```

Next, we can use the write() function to add text to our file. This function takes in a String as a parameter, and we can use the `appendLine()` function to add a new line after each string.

```
fileWriter.write("Hello")
fileWriter.write("World")
fileWriter.appendLine()
fileWriter.write("This is a new line")
```

Finally, we need to close the fileWriter object to save and close our file.

```
fileWriter.close()
```

Once we have completed these steps, our text file should now contain the strings "Hello" and "World" on the first line, and "This is a new line" on the second line.

## Deep Dive
One important thing to note is that when writing to a file, we should always handle any possible exceptions. For example, the `write()` function throws an IOException, which means that if something goes wrong while writing to the file, the code will stop executing. To avoid this, we can wrap our code in a try-catch block and handle any potential errors.

Another useful function when writing to a text file is `flush()`, which ensures that all data is written to the file before closing it. This is especially important if we are writing time-sensitive data and want to avoid any delays.

## See Also
- [Kotlin FileWriter documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file-writer/)
- [Kotlin appendLine() function documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/append-line.html)
- [Java FileWriter documentation](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)