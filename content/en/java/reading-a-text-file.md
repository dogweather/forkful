---
title:    "Java recipe: Reading a text file"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
Reading a text file may seem like a basic task in Java programming, but it is a crucial skill to have. Most programs require some form of user input, and reading files is a common way to obtain that input. It is also useful for data analysis and manipulation, making it an essential skill for any aspiring Java programmer.

## How To
Reading a text file in Java involves several steps:

1. Declare a `File` object and provide the path to the file as a parameter.
2. Create a `Scanner` object and pass the `File` object as a parameter.
3. Use a `while` loop to iterate through the file until the end is reached.
4. Inside the loop, use the `nextLine()` method to read each line of the file.
5. You can then store the data in a variable, print it to the console, or perform any desired operations on it.

Let's take a look at a simple example:

```Java
import java.io.File;
import java.util.Scanner;

public class ReadFile {
    public static void main(String[] args) {
        try {
            File file = new File("example.txt"); //replace with your file path
            Scanner input = new Scanner(file);

            while (input.hasNextLine()) {
                String data = input.nextLine();
                System.out.println(data);
            }

            input.close();
        } catch (Exception e) {
            System.out.println("File not found");
        }
    }
}
```

**Output:**
```
This is the first line of the file.
This is the second line of the file.
This is the third line of the file.
```

In this example, we are using the `File` and `Scanner` classes to read and display the contents of a text file. By using a `while` loop, we are able to print each line of the file until the end is reached. It is important to note that we are using a `try...catch` block to handle any potential errors, such as a file not being found.

## Deep Dive
Reading a text file in Java can also involve manipulating the data in some way, such as searching for specific keywords, counting the number of lines, or parsing the data into different variables. This can be achieved by using methods such as `contains()`, `length()`, and `split()`. It is also possible to read different types of files, such as CSV or JSON, by using specialized libraries or classes.

It is important to properly handle and close the file in order to prevent any data loss or corruption. In the example above, we are using the `close()` method to close the `Scanner` object once we are done using it. If the file is not closed, it can lead to memory leaks and cause the program to crash.

## See Also
If you want to learn more about reading text files in Java, here are some helpful resources:

- [Oracle's Java Tutorials on Reading and Writing Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java Documentation for the File Class](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/io/File.html)
- [Java Documentation for the Scanner Class](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Scanner.html)

Happy coding!