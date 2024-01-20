---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in programming involves creating and saving data to a readable file format, like .txt. Programmers do this to persist data, configure systems, log information, or export human-readable content.

## How to:

Let's write "Hello, file!" to a "greeting.txt" file.

```Kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, file!"
    File("greeting.txt").writeText(textToWrite)
}
```

After running:
```
Hello, file! (in greeting.txt)
```

What if we need to append text instead of overwriting?

```Kotlin
fun appendTextToFile(filename: String, text: String) {
    File(filename).appendText("\n$text")
}

fun main() {
    appendTextToFile("greeting.txt", "Another line!")
}
```

Result in `greeting.txt`:
```
Hello, file!
Another line!
```

## Deep Dive

Historically, text files have been a cornerstone in configuring and logging within software systems. While tools and formats (like XML, JSON) have evolved, text files remain a simple, universally accessible method to interact with data.

Alternatives to `java.io.File` include `java.nio.file.Files` and `java.io.FileWriter`, offering more control and efficiency for larger files or more complex operations.

Key implementation details:
- **Encoding**: By default, `writeText` uses UTF-8 encoding. For different encoding, use `writeText(textToWrite, Charsets.ISO_8859_1)` or similar.
- **Buffering**: When working with larger files, remember to buffer. Wrap your writer in a `BufferedWriter` for better performance.
- **Exception Handling**: Be aware of potential `IOExceptions` and handle them accordingly.

## See Also

- Official Kotlin Documentation on File IO: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- `java.nio.file` package for modern file I/O: [Java Docs](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- Learn about `BufferedWriter` for efficient writing: [Java BufferedWriter](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)