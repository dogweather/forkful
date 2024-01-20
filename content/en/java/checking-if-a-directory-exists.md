---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:56:39.519215-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists means verifying it's there before you try to read or write files in it. Programmers do this to avoid errors, like trying to save a file where there's no place to put it.

## How to:
Here's how you check if a directory exists with `java.nio.file`:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class DirectoryCheck {

    public static void main(String[] args) {
        Path directoryPath = Path.of("/path/to/directory");

        // Check if the directory exists
        boolean directoryExists = Files.exists(directoryPath);

        // Print the result
        System.out.println("Does the directory exist? " + directoryExists);
    }
}
```

If you run this, your console will simply show:

```
Does the directory exist? true // or false
```

Knock yourself out.

## Deep Dive
Back in the day, people used the `java.io.File.exists()` method. But `java.nio.file.Files.exists(Path)` is the hotshot now because it's more versatile. You can also check file attributes with the same API.

But wait, there's more. The `Files.exists` method is not bulletproof—there's a race condition. What if something happens to the directory right after you check? Boom, your operation fails. To mitigate this, use `Files.exists` sparingly and handle exceptions properly when doing actual file operations.

Alternatively, you could simply attempt the file operation and catch the possible `NoSuchFileException`. This is known as "easier to ask for forgiveness than permission" (EAFP) versus "look before you leap" (LBYL), which is what `Files.exists()` is doing.

## See Also
- [Files.exists()](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...))
- [File I/O in Java](https://docs.oracle.com/javase/tutorial/essential/io/)
- A cool article about EAFP vs. LBYL: [The EAFP Principle](https://devblogs.microsoft.com/python/idiomatic-python-eafp-versus-lbyl/)