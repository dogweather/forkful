---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Reading a Text File in Java: A Practical Guide

## What & Why?

A process of extracting data from a .txt file is referred to as reading a text file. Programmers do this to gather, manipulate, or analyze the existing information without manually inputting it into the program.

## How to:

Let's get practical with an example of how to read a text file in Java:

```Java
import java.nio.file.*;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String fileName = "file_example.txt";
        
        try { 
            String content = Files.readString(Paths.get(fileName));
            System.out.println(content);
        } catch(IOException e) {
            e.printStackTrace();
        }
    }
}
```

Let's say that `file_example.txt` contains the text `Hello, World!`. Here's what will print:

```Java
Hello, World!
```

Easy, right?

## Deep Dive

For historical context, before Java 11, reading a text file was a bit more complex, involving loops and buffered readers.

Java 11 introduced `Files.readString()` and `Files.readAllLines()`, making life much easier. Still, the older methods remain relevant, particularly in scenarios where you want more granular control over the process, like reading a file line by line.

As alternatives, third-party libraries like Apache Commons IO and Google's Guava also provide text-reading functionality with added features.

If you dive deep into `Files.readString()`, you'll find that it uses the UTF-8 character encoding by default. Be mindful of this if you're working with text files written in a different encoding.

## See Also

- Oracle's Official Java Documentation on File I/O ([link](https://docs.oracle.com/javase/tutorial/essential/io/file.html))
- Tutorial on reading and writing text files before Java 11 ([link](https://www.baeldung.com/java-read-lines-large-file))
- Apache Commons IO ([link](https://commons.apache.org/proper/commons-io/))
- Google's Guava ([link](https://github.com/google/guava/wiki))