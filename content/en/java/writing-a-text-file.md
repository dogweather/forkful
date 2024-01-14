---
title:    "Java recipe: Writing a text file"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why Text Files Are Important in Java Programming
Text files are a crucial aspect of programming in Java, allowing us to store and manipulate data on our computer's filesystem. Being able to read and write text files is a fundamental skill for any Java programmer, as it allows for persistent storage of information and facilitates communication between different programs.
 
## How To Write Text Files in Java
To write a text file in Java, we first need to import the `java.io` package, which contains the necessary classes for file input and output. Then, we can use the `FileWriter` class to create a new file, specify the file path and name, and write data to it using the `write()` method. Here is a simple code example:

```Java
import java.io.FileWriter;

public class TextFileWriter {
    public static void main(String[] args) {
        try {
            // create a new file
            FileWriter fileWriter = new FileWriter("mytextfile.txt");

            // write data to the file
            fileWriter.write("Hello there! This is a text file written in Java.");

            // close the file
            fileWriter.close();

            System.out.println("Text file successfully written!");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
```
Running this code will create a file named `mytextfile.txt` in the same directory as the Java file, and the text "Hello there! This is a text file written in Java." will be written to it.

## Deep Dive into Writing Text Files
When writing text files, it's important to consider encoding. By default, Java uses the system's default character encoding, but this can cause issues if you need to read the file on a different system with a different encoding. To avoid this, we can specify the encoding when creating the `FileWriter` object, like this:

```Java
FileWriter fileWriter = new FileWriter("mytextfile.txt", StandardCharsets.UTF_8);
```

It's also important to close the file after writing to it, as this will release any system resources used and ensure the file is saved properly. For this, we use the `close()` method, as shown in the code example above.

Another useful class for writing text files is the `BufferedWriter` class, which allows for more efficient writing of large amounts of data to a file. By using `BufferedWriter`, the data will be saved to the file in larger chunks instead of one character at a time.

## See Also
- JavaDocs for `java.io.FileWriter`: https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html
- JavaDocs for `java.io.BufferedWriter`: https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html
- Tutorial on reading and writing text files in Java: https://www.baeldung.com/java-write-to-file