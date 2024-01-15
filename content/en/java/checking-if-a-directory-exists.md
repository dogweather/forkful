---
title:                "Checking if a directory exists"
html_title:           "Java recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Why

Checking if a directory exists is a crucial task for every Java developer. It allows them to ensure that their code can handle all possible scenarios, even if the directory may not exist.

##How To

To check if a directory exists in Java, you can use the `File` class. Simply create an instance of the `File` class with the directory path as the argument. Then, use the `exist()` method to check if the directory exists.

```Java
File directory = new File("C:\\Users\\mydirectory");
if(directory.exists()) {
  System.out.println("The directory exists.");
}
```
Output: The directory exists.

You can also use the `isDirectory()` method to ensure that the given path is a directory and not a file. This method returns a boolean value of `true` or `false`.

```Java
File directory = new File("C:\\Users\\myfile.txt");
if(directory.isDirectory()) {
  System.out.println("The given path is a directory.");
}
```
Output: The given path is not a directory.

##Deep Dive

When using the `File` class, it is essential to handle exceptions that may occur when performing operations like checking if a directory exists. These exceptions include `IOException`, `SecurityException`, and `NullPointerException`. It is recommended to use a `try-catch` block to handle these exceptions and prevent any unexpected behavior in your code.

Another aspect to consider is that the `exists()` method may return `true` even if the directory is not accessible or readable. Therefore, it is essential to also check the directory's permissions using the `canRead()` and `canWrite()` methods.

##See Also

- [Oracle File class documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java File Handling tutorial](https://www.baeldung.com/java-file-handle)