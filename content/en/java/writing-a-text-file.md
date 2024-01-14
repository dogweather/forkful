---
title:                "Java recipe: Writing a text file"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may have come across the need to store large amounts of data in a structured format. One way to do this is by writing a text file. Text files are widely used in various programming languages and can be easily read and written by both humans and machines.

## How To

To write a text file in Java, we will be using the `FileWriter` class. First, we need to import the necessary package:

```Java
import java.io.FileWriter;
```

Next, we will declare a `try-catch` block to handle any potential errors:

```Java
try {
    // code to be executed
} catch (IOException e) {
    // code to handle exception
}
```

Within the `try` block, we will create an instance of the `FileWriter` class and specify the name and location of the file we want to write to:

```Java
FileWriter fileWriter = new FileWriter("myFile.txt");
```

Then, we can use the `write()` method to write our desired content to the file:

```Java
fileWriter.write("Hello world!");
```

After writing to the file, we need to close it using the `close()` method:

```Java
fileWriter.close();
```

This will ensure that all the data is written to the file and that any resources used are released.

## Deep Dive

The `FileWriter` class is a character stream that allows us to write character-oriented data to a file. It inherits from the `Writer` class and provides additional methods for writing strings, characters, and arrays to a file.

When writing to a text file, we can also specify whether we want to append the text to the existing content or overwrite it. This can be done by passing a `boolean` value in the constructor of the `FileWriter` class. For example:

```Java
FileWriter fileWriter = new FileWriter("myFile.txt", true);
```

This will append the new content to the end of the existing content in the file.

We can also use the `append()` method to append text to a file without having to create a new instance of the `FileWriter` class. For example:

```Java
fileWriter.append("Hello again!");
```

Additionally, we can also use the `write()` method with a `char` array or a portion of a `char` array to write larger chunks of data to a file.

## See Also

- [Oracle's Java File I/O tutorial](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java FileWriter documentation](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [How to Read and Write to a Text File in Java](https://www.baeldung.com/java-write-to-file)