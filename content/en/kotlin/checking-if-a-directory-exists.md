---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Ever ran into frustrating errors due to non-existing directories in your programs? Well, that's where the need to check if a directory exists in Kotlin comes in. It’s easy and prevents bugs and crashes arising from unavailable directories.

## How to:
Checking if a directory exists in Kotlin is done using the `exists()` method of the `java.io.File` class, as shown in the sample code below:

```Kotlin
import java.io.File

fun main() {
    val dir = File("/path/to/your/directory")

    if(dir.exists()){
        println("Directory exists!")
    } else {
        println("Directory doesn't exist!")
    }
}
```

When you run this program, it will print "Directory exists!" if the directory exists, and "Directory doesn't exist!" if it doesn't.

## Deep Dive
This technique of checking if a directory exists in Kotlin has been prevalent and largely unchanged since the early releases of the Java platform, on which Kotlin is based. It's straightforward and fits most cases.

For alternatives, Kotlin also offers the extension function `File.isDirectory()`. This method not only checks if the path exists, but also if it's a directory and not a file:

```Kotlin
if(dir.isDirectory){
    println("Directory exists!")
} else {
    println("It either doesn't exist or isn't a directory!")
}
```

Lastly, let's dive into the workings of `exists()`. It's a native method implemented in Java's File class and extends to Kotlin. It essentially makes a system call to access the file directory, returning a Boolean based on the outcome.

## See Also
For more reading and resources related to file and directory operations, refer to the following sources:

- [Kotlin's official Java interoperability documentation](https://kotlinlang.org/docs/java-interop.html)
- [Java's official File class documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)