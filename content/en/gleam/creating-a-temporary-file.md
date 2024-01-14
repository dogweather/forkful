---
title:    "Gleam recipe: Creating a temporary file"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Creating a temporary file may not seem like an exciting task, but it is a crucial one in the world of programming. Temporary files serve as a temporary storage location for data or as a placeholder for future files. They are essential for the smooth functioning of many programs and can help optimize memory usage.

## How To

To create a temporary file in Gleam, we can use the `Temp` module provided by the standard library. First, we need to import the module using `import Temp` at the top of our code.

Next, we can use the `file` function from the `Temp` module to create a temporary file. This function takes in two arguments - a prefix for the file name and a suffix for the file extension. For example, if we want to create a temporary file with the name "tempFile.txt", we can use the following code:

```Gleam
let temp_file = Temp.file("tempFile", ".txt")
```

This will create a temporary file in the default temporary directory and return a `Result` type that contains a `File` object if the file was successfully created.

We can then use the `write` function from the `File` module to write data to our temporary file. For example, if we want to write the text "Hello World!" to our temporary file, we can use the following code:

```Gleam
File.write(temp_file, "Hello World!")
```

We can also use the `close` function from the `File` module to close our temporary file once we are done using it. This will free up any system resources being used by the file.

## Deep Dive

Under the hood, creating a temporary file involves several steps. When we call the `Temp.file` function, Gleam checks for the default temporary directory and generates a unique file name using the provided prefix and suffix. It then creates an empty file with this name and returns it to us. The `File` object returned by the `Temp.file` function also contains information about the file's absolute path, size, and permissions.

It is also worth noting that the `Temp` module provides additional functions for creating temporary directories, as well as randomly generated temporary file names. These can be useful for more complex applications that require a larger number of temporary files.

See Also

- Gleam standard library documentation for `Temp`: https://gleam.run/core/Temp.html
- Gleam tutorial on file IO: https://gleam.run/book/tour/file_io.html