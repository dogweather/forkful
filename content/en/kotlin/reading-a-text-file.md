---
title:                "Kotlin recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a common and important task in any programming language. Whether you're trying to retrieve data from a file or process a large dataset, knowing how to read a text file is a valuable skill for any programmer. In this blog post, we'll explore how to read a text file using Kotlin programming language.

## How To

To read a text file in Kotlin, we first need to create a file object that represents our text file. We can do this using the `File` class from the standard library.

```
val file = File("file.txt")
```

Next, we need to create a `Scanner` object that will allow us to read the contents of the file.

```
val scanner = Scanner(file)
```

Now, we can use the `while` loop to read each line of the file until we reach the end of the file.

```
while(scanner.hasNextLine()) {
    println(scanner.nextLine())
}
```

This code will print out each line of the file to the console. We can also store the lines in a list or perform any other operations on each line.

```
val lines = mutableListOf<String>()
while(scanner.hasNextLine()) {
    lines.add(scanner.nextLine())
}
```

Alternatively, we can use the `useLines` function to read the file and perform operations on each line in a functional way.

```
val lines = file.useLines { lines -> lines.toList() }
```

Additionally, Kotlin provides the `readLines` function to read all the lines of a file at once and store them in a list.

```
val lines = file.readLines()
```

## Deep Dive

When reading a text file, it's important to consider things like encoding and handling of line endings. By default, the `Scanner` object uses the system's default encoding. However, we can specify the encoding explicitly by passing it as a parameter.

```
val scanner = Scanner(file, Charset.forName("UTF-8"))
```

Kotlin also provides ways to handle different line endings, such as `useDelimiter` function to set a specific delimiter for the lines or `nextLine` function with a specified delimiter to read until that delimiter is found.

```
scanner.useDelimiter(",")
while(scanner.hasNextLine()) {
    println(scanner.nextLine())
}
```

Additionally, we can use `forEachLine` function to iterate over each line of the file and perform operations on it.

```
file.forEachLine { line ->
    println(line)
}
```

## See Also

Here are some additional resources to help you dive deeper into reading text files in Kotlin:

- [Kotlin IO Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/)
- [Kotlin Collections](https://kotlinlang.org/docs/reference/collections-overview.html)
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)