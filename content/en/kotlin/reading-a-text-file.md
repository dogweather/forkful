---
title:    "Kotlin recipe: Reading a text file"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're new to Kotlin programming, you might be wondering why you would need to learn how to read a text file. Well, text files are often used to store data that can be easily read and modified by both humans and computers. Being able to read and manipulate text files is a useful skill for any developer, regardless of the programming language they are using.

## How To 

In Kotlin, reading a text file is a relatively straightforward process. First, you need to create a `File` object and pass in the name and path of the text file you want to read. Then, you can use the `inputStream()` function to create an `InputStream` object, which allows you to read the contents of the file. Finally, you can use `readText()` to read the entire file as a `String`. Here's an example of how this would look in code:

```Kotlin
fun main() {
  val file = File("test.txt")
  val inputStream = file.inputStream()
  val text = inputStream.readText()
  println(text)
}
```

Assuming that the "test.txt" file contains the phrase "Hello World", the output of this code would be "Hello World". 

## Deep Dive

If you want to read a text file line by line instead of reading the entire file as a `String`, you can use the `forEachLine()` function. This function takes in a lambda expression as a parameter and executes it for each line in the file. Here's an example:

```Kotlin
fun main() {
  val file = File("test.txt")
  file.forEachLine { line ->
    println(line)
  }
}
```

This code would print out each line in the "test.txt" file. Additionally, you can use the `BufferedReader` class to read a text file. This class provides more efficient reading capabilities, especially for large files.

## See Also

- [Kotlin File Class Reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java InputStream Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/InputStream.html)
- [Kotlin Iterable forEach Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/-iterable/for-each.html)