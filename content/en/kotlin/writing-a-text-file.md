---
title:                "Kotlin recipe: Writing a text file"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Why Write A Text File in Kotlin?

Writing a text file using Kotlin can be a useful skill to have when working on projects that require data storage or transfer. It allows you to easily store and manipulate data without needing a database. Additionally, it can be a handy way to save user inputs or logs in your applications. In this blog post, we will explore how to write a text file in Kotlin and the deeper aspects of this process.

## How To Write A Text File using Kotlin

To write a text file using Kotlin, we will use the ```FileWriter``` class from the ```java.io``` package. This class provides methods to write characters or strings to a file. Let's take a look at a simple example:

```
fun main() {
    val text = "This is a sample text to be written into a file."

    val file = FileWriter("sample.txt")
    file.write(text)

    file.close()
}
```

In the above code, we have created a string variable named ```text``` which contains the data we want to write to the file. Next, we use the ```FileWriter``` class to create a new file named "sample.txt". Then, we use the ```write()``` method to pass in our string variable which will be written to the file. Finally, we close the file to save the changes.

Now, let's take a look at the contents of the "sample.txt" file:

```
This is a sample text to be written into a file.
```

You can see that our text has been successfully written to the file. You can also use the ```append()``` method to add more data to the file instead of overwriting it.

## Deep Dive into Writing a Text File

While the above example shows a simple way to write a text file, there are a few things to keep in mind when working with this process. Firstly, make sure to handle any exceptions that may occur, such as file not found or permission issues. This can be done by using the ```try-catch``` block.

You can also use other methods from the ```FileWriter``` class such as ```write(char[] cbuf, int off, int len)``` to write specific chunks of data from a character array, or ```write(String str, int off, int len)``` to write specific parts of a string.

Additionally, you can use the ```BufferedWriter``` class to improve performance by writing data to a buffer and then flushing it to the file all at once. This is especially useful when dealing with large amounts of data.

# See Also

- [Kotlin Documentation on FileWriter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-writer.html)
- [Java Documentation on FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Kotlin Documentation on BufferedWriter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-writer.html)
- [Java Documentation on BufferedWriter](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html)