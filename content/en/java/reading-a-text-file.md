---
title:                "Java recipe: Reading a text file"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Are you new to Java programming and wondering why someone would need to read a text file? Well, reading a text file is a fundamental task in programming as it allows you to access and manipulate the data within the file. This can be useful for tasks such as data analysis, data processing, and data storage.

## How To

To read a text file in Java, you will need to use the FileReader class and wrap it with a BufferedReader class for efficient reading. Here's an example code for reading a text file called "data.txt" and printing its contents to the console:

```Java
import java.io.*;

public class ReadFile {
    public static void main(String[] args) {
        try {
            FileReader fileReader = new FileReader("data.txt");
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            String line;
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }

            bufferedReader.close();
        } catch (IOException e) {
            System.out.println("File not found!");
        }
    }
}
```

Assuming the "data.txt" file contains the text "Hello World", the output of this code would be:

```
Hello World
```

This code first creates a FileReader object and passes the text file's path as a parameter. Then, it wraps the FileReader with a BufferedReader object to read the file efficiently using the readLine() method. The while loop iterates through the lines in the file and prints them to the console until there are no more lines to read. Finally, the BufferedReader is closed to release the resources.

## Deep Dive

Apart from just reading the content of the file, you can also perform different operations on the data. For example, you can use the split() method to separate the data into different variables, or use the substring() method to extract certain parts of the data. You can also use the StringTokenizer class to tokenize the content of the file and use it for further processing.

It's also important to handle exceptions when reading a text file in Java. In our example code, we used a try-catch block to handle the IOException, which can occur if the file is not found or cannot be read.

## See Also

- [Reading and Writing Text Files in Java](https://www.baeldung.com/java-read-file)
- [Java File IO Tutorial](https://www.tutorialspoint.com/java/java_files_io.htm)
- [BufferedReader Class in Java](https://www.geeksforgeeks.org/bufferedreader-class-in-java/)

Happy coding!