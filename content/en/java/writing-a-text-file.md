---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Java means storing text data to a file on disk. Developers do it for tasks like logging, configuration, or exporting human-readable data.


## How to:

With Java's `java.nio.file` package, writing to a text file is simple. Check out `Files.write()` for a quick save:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class WriteTextFileExample {
    public static void main(String[] args) {
        List<String> lines = List.of("Line 1", "Line 2", "Line 3");
        Path file = Path.of("example.txt");

        try {
            Files.write(file, lines);
            System.out.println("Successfully written to the file.");
        } catch (IOException e) {
            System.err.println("Oops! An error occurred: " + e.getMessage());
        }
    }
}
```

Output:
```
Successfully written to the file.
```

## Deep Dive

In the good old days, Java I/O was all about `FileWriter` and `BufferedWriter`. Now, the NIO package (`java.nio.file`) is the go-to. `Files.write()` is nifty—handles creation, opening, and writing in one go. Alternative? `FileOutputStream` for byte-level control. Under the hood, `Files.write()` uses a `BufferedWriter` and `Charset` to encode text as bytes.


## See Also

Dive deeper into file I/O with these links:

- Official `java.nio.file.Files` documentation: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Oracle's guide on file I/O: https://docs.oracle.com/javase/tutorial/essential/io/
- For a byte-oriented approach, explore `FileOutputStream`: https://docs.oracle.com/javase/8/docs/api/java/io/FileOutputStream.html
