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

## Why

Writing a text file is a common task in programming that allows you to easily store and manipulate data in a simple and readable format. Whether you're creating a configuration file, saving user input, or outputting results, knowing how to write a text file is an essential skill for any developer.

## How To

To write a text file in Kotlin, you can use the `FileWriter` class. First, create a `File` object to represent the file you want to write to, using the path to the file as a parameter. Then, use the `appendText()` method to add content to the file. Here's an example:

```Kotlin
// Create File object for the file "sample.txt"
val file = File("sample.txt")
// Use FileWriter to append text to the file
file.appendText("Hello world!")
```
This will create the file "sample.txt" and add the text "Hello world!" to it. You can also use string interpolation to insert variables into the text you're writing, like this:

```Kotlin
val name = "Jane"
file.appendText("Hello $name, welcome to my program!")
```

An alternative and more concise way of writing to a file is by using the `printWriter()` function. This function automatically creates the file if it doesn't exist and overwrites its contents if it does. Here's an example of how you can use it:

```Kotlin
// Use extension function printWriter to write to the file "sample.txt"
File("sample.txt").printWriter().use { out ->
    out.println("This is a new line")
    out.println("This is another line")
}
```

The `use` function ensures that the writer is properly closed after the code block is executed.

## Deep Dive

When writing to a text file, it's important to consider the encoding used for the file. By default, the `printWriter()` function uses the system's default encoding, but you can specify a different one as a parameter. For example, to use UTF-8 encoding for the file, you can use the following code:

```Kotlin
// Use UTF-8 encoding when writing to the file
File("sample.txt").printWriter(Charsets.UTF_8).use { out ->
    out.println("This is written in UTF-8 encoding")
}
```

It's also important to note that when writing to a file, the content is buffered in memory before being written to the file in one go. This can be a problem when writing large amounts of data. To avoid this, you can use the `appendText()` function, which automatically flushes the buffer after each write.

## See Also

- [Kotlin File APIs](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file.html)
- [Kotlin Standard Functions](https://kotlinlang.org/docs/reference/lambdas.html#function-composition)
- [Java FileWriter class](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)