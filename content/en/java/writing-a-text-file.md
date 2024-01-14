---
title:    "Java recipe: Writing a text file"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why

Text files are an essential part of programming and are commonly used for storing and accessing data. By knowing how to write a text file in Java, you can create and manipulate various types of data, making your programs more versatile and flexible.

## How To

Writing a text file in Java involves a few simple steps:

1. Import the `java.io` package to access the necessary classes and methods for working with files.
2. Create a `FileWriter` object and specify the file path and name.
3. Use the `write()` method to add content to the file. This method takes in a string or char array as a parameter.
4. Remember to use the `newLine()` method to add line breaks between each piece of content.
5. Close the `FileWriter` object using the `close()` method to save and finalize the file.

Let's take a look at an example of writing a text file called "sample.txt":

```Java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {
        // create a FileWriter object
        FileWriter writer = null;
        try {
            writer = new FileWriter("sample.txt");

            // add content to file
            writer.write("Hello");
            writer.newLine();
            writer.write("This is a sample text file");
            writer.newLine();
            writer.write("Written using Java");

            // close FileWriter object
            writer.close();
            System.out.println("File created successfully.");
        } catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}
```

If we run this code, a new text file named "sample.txt" will be created in the same directory as our Java file. The content we specified will be written to the file, with each piece of content on a new line. The console will also display a message confirming that the file was created successfully.

## Deep Dive

When working with text files, it's important to handle potential errors that may occur. This is why we use the `try-catch` block in our code. The `IOException` in the `catch` statement handles any input/output errors, such as the file not being found or unable to be written to.

It's also worth mentioning that using the `write()` method does not automatically add a new line. This is why we use the `newLine()` method to add line breaks between each piece of content.

Furthermore, the `FileWriter` class also has a `append()` method that allows us to add new content to an existing file without overwriting it.

## See Also

To learn more about working with text files in Java, check out the following resources:

- [Reading and Writing Files in Java](https://www.baeldung.com/java-write-to-file)
- [Java FileWriter class](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Handling Errors in Java](https://www.geeksforgeeks.org/try-catch-block-in-java/)

Now that you know how to write a text file in Java, you can use this knowledge in your future projects to store and manipulate data. Happy coding!