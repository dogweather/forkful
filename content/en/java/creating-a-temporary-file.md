---
title:                "Java recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files may seem like a tedious task, but it can actually be quite useful in certain programming scenarios. Temporary files are often used for storing intermediate data or as a placeholder for data that will be written to a more permanent location. They can also help improve the overall performance of a program by reducing file I/O operations.

## How To
To create a temporary file in Java, we can use the `File.createTempFile()` method. This method takes in two parameters: a prefix for the file's name and a suffix for the file's extension. Let's take a look at an example:

```Java
public static void main(String[] args) {
    try {
        // Create a temporary file with prefix "myFile" and suffix ".txt"
        File tempFile = File.createTempFile("myFile", ".txt");

        // Print out the path of the temporary file
        System.out.println("Temporary file path: " + tempFile.getAbsolutePath());
    } catch (IOException e) {
        // Handle exception
    }
}
```

Running this code will output something like `Temporary file path: /var/folders/06/5hmwf54d0gzfj34nxhn8f55m0000gn/T/myFile8738813599185034465.txt`, as the prefix and suffix are combined with a randomly generated number to create a unique file name. The temporary file will be created in the default temporary directory of the operating system.

You can also specify the directory location where you want the temporary file to be created by passing in a `File` object as the third parameter. For example:

```Java
// Create a temporary file in the specified directory
File tempFile = File.createTempFile("myFile", ".txt", new File("/Users/username/Desktop/temp"));
```

Once you have finished using the temporary file, you should delete it using the `File.delete()` method. It is good practice to do this within a `finally` block to ensure that the file is always deleted, even if an exception is thrown.

## Deep Dive
Behind the scenes, the `File.createTempFile()` method actually uses the `java.io.File` class as a wrapper for the temporary file. The wrapper file is responsible for creating and deleting the temporary file, and it is also used to access the temporary file's path and other information.

It is worth noting that the contents of a temporary file are not automatically deleted when the JVM exits. The temporary file will only be deleted if the `File.deleteOnExit()` method is called. However, this method should be used with caution as it can cause problems if the temporary file is not properly closed.

## See Also
- [Java File Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Official Java Tutorial on Working with File Systems](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)