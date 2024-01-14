---
title:                "Kotlin recipe: Checking if a directory exists"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

As a programmer, it is important to ensure that our code is robust and can handle various scenarios. One such scenario is when we need to check if a directory exists before performing any operations on it. This helps us avoid errors and handle potential issues before they occur.

## How To

To check if a directory exists in Kotlin, we can use the `exists()` function from the `java.io.File` class. Let's take a look at an example:

````Kotlin

import java.io.File

fun main() {

    val directory = File("my_directory")

    if(directory.exists()) {
        println("Directory exists")
    } else {
        println("Directory does not exist")
    }
}

````

In this code, we first create a `File` object that represents our directory. Then, we use the `exists()` function to check if it exists. Depending on the result, we print out a corresponding message.

Running this code will give us the following output:

```
Directory does not exist
```

Now, let's assume that the directory does exist. In that case, we will get this output:

```
Directory exists
```

It's as simple as that! We can also use the `exists()` function in combination with other file handling functions, to create, read, and perform other operations on a directory if it exists.

## Deep Dive

Under the hood, the `exists()` function checks if the specified file or directory exists in the file system. It returns a boolean value, `true` if it exists and `false` if it does not. This function also takes into consideration permissions and other factors that may affect the accessibility of a file or directory.

It is important to note that just because a directory exists, it does not necessarily mean that we have permission to access it. Therefore, it is a good practice to handle exceptions and errors while checking for a directory's existence.

## See Also

Here are a few links to resources you can refer to for further reading:

- [Kotlin docs on java.io.File class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Official Android documentation on File handling](https://developer.android.com/training/data-storage/files)
- [Stack Overflow thread on checking if a directory exists in Kotlin](https://stackoverflow.com/questions/12802926/java-checking-if-a-directory-exists-linux/git/61835246#61835246)