---
title:    "Kotlin recipe: Writing a text file"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file may seem like a simple task, but it is a fundamental skill for any programmer. From storing data to saving user inputs, text files are an essential part of developing any application.

## How To
Writing a text file in Kotlin is a straightforward process. First, we need to import the `java.io` package to access the necessary classes and methods. Then, we can use the `FileWriter` and `BufferedWriter` classes to create and write to a text file. Let's take a look at an example:

```
Kotlin
import java.util.*
import java.io.*

fun main() {
    // Create a file object
    val file = File("my_text_file.txt")

    // Open a FileWriter and BufferedWriter objects
    val fw = FileWriter(file)
    val bw = BufferedWriter(fw)

    // Write a string to the file
    bw.write("This is a sample text file.")

    // Close the BufferedWriter
    bw.close()
}
```

Running the code above will create a text file named `my_text_file.txt` and write the string "This is a sample text file." to it. We can also add the optional parameter `true` to the `FileWriter` object to append the text to an existing file instead of overwriting it.

## Deep Dive
While the example above covers the basics of writing a text file in Kotlin, there are other ways to achieve the same result. For instance, we can use the `println()` function to write to a file, but this is not as efficient as using `BufferedWriter`. Additionally, when writing large amounts of data, using `BufferedWriter` would be the preferred method.

It is also essential to handle exceptions when writing a text file. The `try-catch` block can be used to catch any potential errors and handle them accordingly.

Another point to consider when working with text files is the character encoding. By default, Kotlin uses the system's default character encoding, but this can be changed by using the optional parameter `charset` in the `FileWriter` constructor.

## See Also
- [Kotlin IO Basics](https://kotlinlang.org/docs/io.html)
- [Writing To A File In Kotlin](https://www.programming-books.io/essential/kotlin/writing-a-text-file-cea98e7483cc4d6f8112eb4559f89fed)

By following the steps outlined in this post, you should now have a good understanding of how to write a text file in Kotlin. Feel free to experiment with different approaches and explore other features of the `java.io` package to enhance your text file writing skills. Happy coding!