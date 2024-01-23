---
title:                "创建临时文件"
date:                  2024-01-20T17:40:22.904938-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
什么以及为什么?

Temporary files are those created to temporarily contain information while a program is running. Programmers create them for tasks like intermediate data processing, which helps in preserving memory and avoiding clutter.

## How to:
怎么做:

Below are simple steps to create a temporary file in Java:

```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Creating a temp file
            File tempFile = Files.createTempFile("myApp", ".txt").toFile();
            System.out.println("Temporary file created: " + tempFile.getAbsolutePath());

            // Deleting the file on exit to avoid clutter
            tempFile.deleteOnExit();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Sample output might be:

```
Temporary file created: C:\Users\...\AppData\Local\Temp\myApp1234567890.txt
```

## Deep Dive
深入了解:

Historically, the need for temporary files dates back to when programs had to deal with limited memory. By using temp files, applications could handle larger data sets or perform complex operations without crashing.

There are alternatives to `Files.createTempFile`, such as `File.createTempFile`, which is older but still valid. `Files.createTempFile` offers more control, like specifying the directory where the temp file will be created.

About implementation: Java uses the system's temporary folder to store temporary files, which are typically deleted automatically. However, it's good practice to call `deleteOnExit()` to ensure the temporary file is deleted when the JVM exits.

## See Also
相关资料:

For more information, visit these links:

- Java Doc for `Files.createTempFile`: https://docs.oracle.com/javase/10/docs/api/java/nio/file/Files.html#createTempFile(java.nio.file.Path,java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...)
- Stack Overflow discussion on temporary files in Java: https://stackoverflow.com/questions/166132/java-io-tmpdir-where-is-it-on-my-machine
- Oracle tutorial on File I/O: https://docs.oracle.com/javase/tutorial/essential/io/file.html
