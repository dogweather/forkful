---
title:                "Creating a temporary file"
html_title:           "Java recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files in Java can be useful in situations where the program needs to store temporary data that is only needed for a short period of time. These files can be deleted once they have served their purpose, freeing up space and resources for other tasks.

## How To
To create a temporary file in Java, you can use the `createTempFile()` method from the `File` class. This method takes in a prefix, suffix, and a `File` object representing the directory where the file should be created. Here is an example:

```Java
File tempFile = File.createTempFile("myFile", ".txt", new File("/temp"));
System.out.println(tempFile.getAbsolutePath());
```

This will create a temporary file named "myFile" with a ".txt" extension in the specified "/temp" directory. The `getAbsolutePath()` method can be used to display the full path of the created file. 

You can also use the `deleteOnExit()` method to ensure that the temporary file will be deleted when the program exits. For example:

```Java
tempFile.deleteOnExit();
```

## Deep Dive
Under the hood, the `createTempFile()` method uses the `createTempFile()` method from the `FileSystem` class to create the temporary file. The `FileSystem` class uses a secure naming scheme to generate unique file names, making it unlikely for multiple programs to accidentally create the same temporary file name.

It is important to note that the `createTempFile()` method creates an empty file with default read and write permissions. If you need to specify different permissions or write data to the file, you can use the `FileOutputStream` or `RandomAccessFile` classes.

## See Also
- [Oracle Java Documentation on temporary files](https://docs.oracle.com/javase/tutorial/essential/io/tempfile.html)
- [JournalDev article on creating temporary files in Java](https://www.journaldev.com/550/java-create-temporary-file)