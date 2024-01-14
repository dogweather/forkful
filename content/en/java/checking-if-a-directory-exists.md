---
title:                "Java recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When working with files and directories in Java, it is important to check if a directory exists before performing any operations on it. This can prevent errors and ensure that the program runs smoothly without any unexpected interruptions.

## How To

To check if a directory exists in Java, we can use the `exists()` method from the `File` class. Here's an example code:

```Java
import java.io.File;

public class DirectoryCheckExample {

    public static void main(String[] args) {
        // Specify the path of the directory to be checked
        String dirPath = "C:/Users/User/Documents/JavaProject";

        // Create a File object
        File directory = new File(dirPath);

        // Check if the directory exists
        if (directory.exists()) {
            // If exists, print a success message
            System.out.println("Directory exists!");
        } else {
            // If doesn't exist, print an error message
            System.out.println("Directory does not exist.");
        }
    }
}
```

Running this code will give the following output:

```Java
Directory exists!
```

In case the directory does not exist, the output will be:

```Java
Directory does not exist.
```

## Deep Dive

Under the hood, the `exists()` method checks if the specified `File` object exists in the file system. It returns a boolean value, `true` if the file exists and `false` if it doesn't.

It is important to note that just because a directory exists, it doesn't necessarily mean that we have access to it. The `exists()` method only checks for the existence of the file, not its permissions. To check for permissions, we can use the `canRead()` and `canWrite()` methods.

## See Also

- [Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [How to Create and Delete Directories in Java](https://www.baeldung.com/java-create-delete-directory)
- [Working with Files and Directories in Java](https://www.geeksforgeeks.org/working-with-files-in-java/)