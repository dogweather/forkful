---
title:                "Writing a text file"
html_title:           "Java recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Java is the process of creating a file that contains text-based data. Programmers often use this technique to store large amounts of information in an organized and readable format. By writing text files, programmers can easily manipulate and access data without complex database systems.

## How to:

To write a text file in Java, you can use the FileWriter and BufferedWriter classes. First, you need to import these classes:

```Java
import java.io.FileWriter;
import java.io.BufferedWriter;
```

Next, you can create an instance of the FileWriter class and specify the file name and location where you want to write the text file. Then, use the BufferedWriter class to write the data to the file. Here's an example:

```Java
FileWriter writer = new FileWriter("myFile.txt"); //creates a new file named myFile.txt
BufferedWriter bufWriter = new BufferedWriter(writer); //creates a buffered writer instance
bufWriter.write("This is an example text file!"); //writes text to the file
bufWriter.close(); //closes the buffered writer
```

This code creates a new text file and writes the text "This is an example text file!" to it. The close() method ensures that the file is saved and closed properly.

## Deep Dive:

Writing text files has been a fundamental part of programming since the early days of computing. It allows programmers to store and access data without the need for complex database management systems. In Java, the FileWriter and BufferedWriter classes have been used for decades to write text files. However, with the introduction of newer technologies such as NoSQL databases, writing text files may not always be the best option.

An alternative to writing text files in Java is using Java Persistence API (JPA), which allows you to store and retrieve data from relational databases. This can be useful if you have a large amount of data that needs to be accessed frequently and in more complex ways. However, for small scale applications, writing text files is still a quick and efficient solution.

When writing a text file in Java, it's important to take into consideration the encoding of the file. By default, Java uses the platform's default charset, but you can specify a specific charset when creating an instance of the FileWriter class. This is important if your text file needs to support different languages or special characters.

## See Also:

- [Java IO Tutorial](https://www.tutorialspoint.com/java/io/index.htm) - Detailed guide on input/output operations in Java.
- [Java Persistence API](https://www.oracle.com/java/technologies/persistence-jsp.html) - Official documentation for JPA.