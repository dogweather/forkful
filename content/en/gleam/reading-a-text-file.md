---
title:                "Reading a text file"
html_title:           "Gleam recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a common task in programming, whether it's to extract data or to process user input. In this article, we'll explore how to read a text file using Gleam and some useful techniques for handling different types of data.


## How To

To read a text file in Gleam, we'll use the `File` module. First, we need to import it into our project using the following code:

```Gleam
import gleam/file
```

Next, we can use the `open_text` function to open a file for reading. This function takes in the file path as a string and returns an `Result` type with the file contents, if successful, or an error if the file cannot be read. Here's an example of how we would use it:

```Gleam
let result = file.open_text("sample.txt")
```

Now, we can use pattern matching to handle the `Result` type and extract the file contents. In the case of a successful read, the `Ok` variant will contain the file contents as a string. We can then print it out or use it in other ways, such as splitting it into lines or parsing it for specific data. Here's an example of how we would print the file contents:

```Gleam
case result {
    Ok(contents) -> println(contents)
    Error(err) -> println(err)
}
```

For more advanced use cases, we can also specify the file mode and encoding when using the `open_text` function. This allows us to read files in different languages or formats, such as UTF-8 or UTF-16. Additionally, we can use the `read_line` function to read a single line from a file, and the `read_all` function to read the entire file as a binary.

## Deep Dive

When working with larger text files, it's important to consider memory usage and error handling. To avoid memory issues, we can use the `read_into` function to read the file into a buffer instead of loading it all into memory at once. This is especially useful for large files that cannot fit into memory.

In terms of error handling, it's best to use the `File` module's `read_line` and `read_all` functions inside a `try` block to catch any potential errors. This will allow for graceful handling of errors and prevent our program from crashing if the file cannot be read.

## See Also

To learn more about working with files in Gleam, check out the official documentation on the `File` module: https://gleam.run/modules/file.html

For more insights on file handling in general, you can also refer to the Rust Programming Language's documentation: https://doc.rust-lang.org/std/fs/struct.File.html