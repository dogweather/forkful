---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file is the process of creating an interim file in Java. Programmers do this for preliminary data storage or testing, before committing data permanently.

## How to:

In Java, it's straightforward to create a temporary file . Simply, you use the `File.createTempFile()` method. Here is a basic script:

```Java
import java.io.File;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            // Create a temporary file
            File tempFile = File.createTempFile("myTempFile", ".txt");

            System.out.println("Temporary file created: " + tempFile.getAbsolutePath());

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
If executed, the above script might produce this kind of output:

```Java
Temporary file created: C:\Users\Username\AppData\Local\Temp\myTempFile4520832760430563571.txt
```

## Deep Dive

The `createTempFile()` method has been a part of Java since JDK 1.2, serving as a reliable way to create non-persistent data file structures. This strategy contrasts with file creation via I/O streams that require more control and explicit deletion.

An alternative way to create a temporary file, if you want a little more control such as defining the directory, is using the two-argument version of the `createTempFile()` method:

```Java
// Create a temporary file in a specified directory
File dir = new File("C:/Dir");
File tempFileInDir = File.createTempFile("myTempFile", ".txt", dir);
```

When a temporary file is created, Java doesn't automatically delete it. You have to do that manually. However, you can make Java to delete the temporary file on exit with `deleteOnExit()`:

```Java
tempFile.deleteOnExit();
```

## See Also

For more information on creating and handling files in Java, see the official documentations:
- [File (Java Platform SE 8 )](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [The Java Tutorials: Basic I/O](https://docs.oracle.com/javase/tutorial/essential/io)