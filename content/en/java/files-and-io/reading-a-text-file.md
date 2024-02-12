---
title:                "Reading a text file"
aliases: - /en/java/reading-a-text-file.md
date:                  2024-01-20T17:54:34.386436-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means your program slurps in content from a file as a string. Programmers do this to process or analyze data sitting in files on their disk. It's bread and butter for tasks like configuration, data analysis, or even just to pull out your to-do list.

## How to:

Reading a file is a breeze in Java, especially with `java.nio.file`. Here's a quick example:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.stream.Stream;

public class FileReadExample {
    public static void main(String[] args) {
        Path filePath = Path.of("example.txt");

        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Running this with `example.txt` containing "Hello, file readers!" would output:

```
Hello, file readers!
```

## Deep Dive

Java has evolved. Back in the day, you'd have to manage streams and readers yourself – plenty of boilerplate. The `java.io` package was all the rage, with `FileReader` and `BufferedReader` often seen in the wild. Then came `java.nio`, offering channels and buffers for more control.

Now, `java.nio.file` is even higher level. `Files` and `Paths` simplify the job. The example above uses `Files.lines`, which streams lines lazily, great for big files. You also get try-with-resources, automatically closing streams to avoid leaks.

Alternatives? `Scanner` is handy for parsing. Apache Commons IO and Google's Guava have utilities for more complex tasks, if you need them. Still, vanilla Java usually gets you pretty far.

Implementation-wise, file encoding matters. `Files.lines` assumes UTF-8 by default but you can specify another. On the other hand, `BufferedReader` needs you to set the `Charset` upfront if it's not the default.

## See Also

For more zest, peek at these:

- The [`Files`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html) class in Java's official documentation.
- [Reading, Writing, and Creating Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html) for a thorough walk-through.
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/) for a robust library of file IO utilities.
