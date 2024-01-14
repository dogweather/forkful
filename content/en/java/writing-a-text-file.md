---
title:                "Java recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files have been a fundamental part of computing for decades. They allow us to store, access, and manipulate data in a straightforward and readable format. Text files can also be easily shared and transferred between different systems, making them a versatile tool for developers. In this post, we will explore the basics of writing a text file using Java, so you can add this valuable skill to your programming toolkit.

## How To

Writing a text file in Java involves a few simple steps. First, we need to import the `FileWriter` and `BufferedWriter` classes from the `java.io` package. Then, we can create an instance of `FileWriter` by passing the name of the file we want to write to as a parameter. Next, we wrap the `FileWriter` in a `BufferedWriter` for better performance.

```Java
import java.io.FileWriter;
import java.io.BufferedWriter;

public class TextFileWriter {
    public static void main(String[] args) {
        // Create instance of FileWriter and BufferedWriter
        FileWriter writer = new FileWriter("sample.txt");
        BufferedWriter bw = new BufferedWriter(writer);

        // Write to file and add new lines
        bw.write("Hello World!");
        bw.newLine();
        bw.write("This is a sample text file.");

        // Close BufferedWriter
        bw.close();
    }
}
```

The above code creates a new text file named `sample.txt` and writes two lines of text to it. You can customize the content of the file by modifying the `write()` statements. It is essential to close the `BufferedWriter` once we are done writing to the file to avoid any data loss.

When the code is executed, it will create a new text file in the same directory as the Java file. Here is the sample output from the code above:

```
Hello World!
This is a sample text file.
```

## Deep Dive

Now that you know the basics of writing a text file in Java, let's dive a little deeper and look at some additional techniques.

### Writing Characters vs. Writing Lines

In the code example above, we used the `bw.write()` method to write our text. This method writes individual characters to the file. Alternatively, we can use the `bw.newLine()` method to write an entire line of text to the file. This method also adds a new line or carriage return to the end of the line, making it useful for formatting multiple lines of text.

### Exception Handling

When working with files, it is essential to handle exceptions effectively. The `FileWriter` and `BufferedWriter` classes can throw `IOExceptions` if there is an issue with reading or writing to the file. Therefore, it is best to wrap our code in a `try-catch` block to handle potential errors gracefully.

## See Also

- [Writing Files in Java](https://www.geeksforgeeks.org/file-handling-java-using-filewriter-bufferedwriter/)
- [Java File Handling](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java BufferedWriter class](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html)