---
title:                "Java recipe: Checking if a directory exists"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Whether you are a beginner or an experienced Java programmer, you may encounter a situation where you need to check if a directory exists. This is an important step to ensure the smooth execution of your code and avoid any potential errors.

## How To
Checking for the existence of a directory in Java is a simple task. First, import the necessary packages:
```Java 
import java.io.File;
```
Next, create a new File object and pass in the directory path as a parameter:
```Java 
File directory = new File("C:/Users/User/Documents");
```
Then, use the `exists` method to check if the directory exists:
```Java 
if(directory.exists()) {
    System.out.println("Directory exists");
} else {
    System.out.println("Directory does not exist");
}
```
The output will depend on whether the directory exists or not.

## Deep Dive
Behind the scenes, the `exists` method uses the `FileSystem` class to access the underlying file system and check for the existence of the directory. It also checks if the user has the necessary permissions to access the directory. If not, the method will return false, even if the directory actually exists.

It's worth noting that the `exists` method only checks for the existence of a directory, not its type (e.g. file or folder). To determine if the path refers to a file or a directory, you can use the `isFile` and `isDirectory` methods respectively.

## See Also
For more information on working with directories in Java, check out these links:
- [Oracle Java Docs: File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [GeeksforGeeks: Java Program to Check if a File or Directory Exists](https://www.geeksforgeeks.org/java-program-to-check-if-a-file-or-directory-exists/)
- [Baeldung: Checking for the Existence of a Directory in Java](https://www.baeldung.com/java-check-directory-exists)