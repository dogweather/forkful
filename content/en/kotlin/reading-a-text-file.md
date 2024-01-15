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

## Why
Reading a text file is a common task for any programmer working with data. Whether it's for parsing data, configuring settings, or simply reading in a file for display, being able to read a text file efficiently can save you time and effort in your coding journey.

## How To
Reading a text file in Kotlin is a simple process that can be accomplished using just a few lines of code. Let's take a look at an example:

```Kotlin
val file = File("data.txt")
file.forEachLine {
    println(it)
}
```

In this example, we first create a `File` object and pass in the name of the file we want to read. Then, we use the `forEachLine` method to iterate through each line in the file and print it out. This is a quick and easy way to read a text file line by line.

But what if we want to store the contents of the file in a variable? We can do that by using the `readText()` method:

```Kotlin
val file = File("data.txt")
val contents = file.readText()
println(contents)
```

In this example, we use the `readText()` method to store the contents of the file in the `contents` variable. Then, we can manipulate or display the contents however we want.

## Deep Dive
There are a few different ways to read a text file in Kotlin, depending on your specific needs. Let's take a closer look at some of the methods that can be used.

#### Using `forEachLine`
As shown in our first example, the `forEachLine` method can be used to easily iterate through each line in a text file. This method is useful for reading large files or files with a lot of data, as it only reads one line at a time.

#### Using `readText`
The `readText` method, as shown in our second example, is useful for reading smaller files or file contents that we want to store in a variable. It reads the entire file at once, so it may not be efficient for large files.

#### Using `readLines`
The `readLines` method is similar to `readText` in that it reads the whole file, but it returns a `List` containing each line of the file as a separate element. This can be useful if you want to manipulate or access individual lines of the file.

#### Using `useLines`
Similar to `forEachLine`, the `useLines` method is used for processing large files and reading each line one at a time. The main difference is that it automatically closes the file once it has been read, which can be useful for managing resources.

## See Also
- [Kotlin File class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Kotlin Standard Library - Read Text Files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Kotlin Standard Library - Read Lines](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html)