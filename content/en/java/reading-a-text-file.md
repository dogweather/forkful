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

## Why
Text files are a common way to store and share data. Reading text files in Java can be a valuable skill for developers, as it allows them to access and manipulate data from external sources.

## How To
Reading a text file in Java can be accomplished by using the built-in File and Scanner classes. The following code shows how to read a text file line by line and print its content:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TextFileReader {
    public static void main(String[] args) {
        // Specify the file path
        File file = new File("/path/to/textfile.txt");
        try {
            // Create a Scanner object to read the file
            Scanner fileReader = new Scanner(file);
            // Read and print each line in the file
            while (fileReader.hasNextLine()) {
                String line = fileReader.nextLine();
                System.out.println(line);
            }
            // Close the scanner
            fileReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found.");
        }
    }
}
```

### Sample Output
```
This is the first line of the text file.
This is the second line of the text file.
This is the third line of the text file.
```

## Deep Dive
The File class represents a file or directory path on your file system. It contains methods for creating, deleting, or accessing information about the file.

The Scanner class is used to read data from different sources, such as files, user input, or strings. In the example above, we created a Scanner object to read from a text file.

In Java, a text file is just a sequence of characters. So, when we read from a text file, we are essentially reading a sequence of characters. The Scanner class helps us to convert these characters into Java types, such as String or int.

One important thing to note is that the Scanner class throws a FileNotFoundException if the specified file does not exist. So, we need to handle this exception in our code.

## See Also
- [Java File class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Scanner class](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Reading and writing text files in Java](https://www.baeldung.com/java-read-write-text-file)