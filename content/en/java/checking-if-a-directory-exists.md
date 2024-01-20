---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Java simply means validating if a specific folder is present on your filesystem. It's essential to prevent errors (e.g., file not found, permission denied), especially before reading files or creating new ones in the directory.

## How to:

We will use the `Files` class in Java's NIO package. Let's get straight to it:

```Java
import java.nio.file.*;

public class CheckDirectory {
  public static void main(String[] args) {
    Path path = Paths.get("/path/to/directory");

    if(Files.exists(path)) {
      System.out.println("The directory exists!");
    } else {
      System.out.println("Directory doesn't exist!");
    }
  }
}
```
If the directory is present at the given path, it will print "The directory exists!", otherwise "Directory doesn't exist!".

## Deep Dive

Historically, we used the `File` class of Java I/O to check a directory's existence. The `File` class methods, however, are deemed less efficient and less versatile compared to `Files` class methods.

Additionally, the `Files` class also offers `Files.notExists(path)`, a bit clearer and more specific.

```Java
if(Files.notExists(path)) {
  System.out.println("Directory doesn't exist!");
} else {
  System.out.println("The directory exists!");
}
```
Or you can add an extra level of checks to look if the path is a directory:

```Java
if(Files.exists(path) && Files.isDirectory(path)) {
  System.out.println("The directory exists!");
}
```
These small, robust methods can handle your directory validity requirements seamlessly and effectively.

## See Also:

1. [Oracle: Java NIO Files class documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
2. [Oracle: Java NIO Path class documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
3. [StackOverflow: How to check if a directory exists in Java](https://stackoverflow.com/questions/3775694/how-to-check-if-a-directory-exists-in-java)