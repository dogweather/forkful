---
title:                "Reading a text file"
html_title:           "Java recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in Java is the process of extracting data from a plain text file and storing it in a program for further manipulation. Programmers often use this technique to read user-provided data or configuration files in their applications.

## How to:

Reading a text file in Java can be easily achieved using the `BufferedReader` class. First, we need to create an instance of the class and pass a `FileReader` object containing the path of the text file we want to read. Then, we can use the `readLine()` method to read each line of the file and store it in a String variable. Here is an example code:

```Java
BufferedReader reader = new BufferedReader(new FileReader("data.txt"));

String line;
while((line = reader.readLine()) != null) {
  System.out.println(line);
}

reader.close();
```

The `readLine()` method returns null when there are no more lines to be read, so we can use it as a condition for our while loop. The `close()` method is used to properly close the file after reading.

## Deep Dive:

Reading text files has been a fundamental aspect of programming since the early days. Before the introduction of object-oriented programming, programmers used to write code in low-level languages like C, where reading a text file involved complex syntax and memory management. Java, with its built-in IO libraries, simplifies this process, making it more accessible for programmers to read data from text files.

Additionally, there are alternatives to using the `BufferedReader` class. For example, the `Scanner` class also provides the ability to read text files with a more straightforward syntax. However, it is slower and not recommended for large files.

When reading a text file, it is essential to consider the character encoding used in the file. Java's default character encoding is UTF-8, so if a file's encoding is different, we need to specify it when creating the `FileReader` object. Otherwise, the data may not be read correctly.

## See Also:

- Java API for `BufferedReader`: https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html
- Java API for `Scanner`: https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html
- Java IO Tutorial: https://www.tutorialspoint.com/java/io/index.htm