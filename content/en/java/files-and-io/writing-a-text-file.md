---
date: 2024-02-03 19:03:21.820214-07:00
description: 'How to: #.'
lastmod: '2024-03-13T22:44:59.988377-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Writing a text file
weight: 24
---

## How to:


### Using `java.nio.file` (Standard Library)
Java's New I/O (NIO) package (`java.nio.file`) provides a more versatile approach for dealing with files. Here's a simplistic way to write to a file using `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("File written successfully!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output:

```
File written successfully!
```

### Using `java.io` (Standard Library)
For a more traditional approach, `java.io.FileWriter` is a good choice for writing text files simply:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hello, World!\n");
            writer.append("This is another line.");
            System.out.println("File written successfully!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Output:

```
File written successfully!
```

### Using Apache Commons IO
The Apache Commons IO library simplifies many operations, including file writing. Here's how to write to a file using `FileUtils.writeStringToFile()`:

First, add the dependency to your project. If using Maven, include:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Check for the latest version -->
</dependency>
```

Then, use the following code to write text to a file:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "This is text written using Commons IO.", "UTF-8");
            System.out.println("File written successfully!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Output:

```
File written successfully!
```
