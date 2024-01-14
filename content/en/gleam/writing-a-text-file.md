---
title:                "Gleam recipe: Writing a text file"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a fundamental task in any programming language. It allows you to store data that can be manipulated and read by both humans and machines. In this blog post, we will explore how to write a text file in the Gleam programming language, and the benefits it provides.

## How To

Writing a text file in Gleam is a simple process. First, we need to import the `gleam/io` library which provides us with functions for working with files.

```Gleam
import gleam/io

// Create a new text file and open it for writing
let output_file = io.file.open("output.txt", ("write"))

// Write data to the file
io.file.write(output_file, "Hello, world!")

// Close the file
io.file.close(output_file)
```

The `open()` function takes two parameters - the file name and the mode. The mode can either be "read", "write" or "append". In our example, we use "write" to create a new file or overwrite an existing one.

Next, we use the `write()` function to add data to the file. In this case, we write the string "Hello, world!" to the `output_file` we created earlier. Finally, we use the `close()` function to free up any resources associated with the file.

To verify that our code worked, we can check the `output.txt` file and see that it now contains the text "Hello, world!".

## Deep Dive

The `write()` function in Gleam takes the file and the data to be written as parameters. However, it is important to note that this function only works with strings. If you want to write other data types, such as integers or booleans, you will need to first convert them to strings using the `to_string()` function.

Additionally, the `write()` function does not automatically add a new line at the end of the data. If you want each write to be on a new line, you will need to manually add the newline character `\n` at the end of the data.

## See Also

- [Gleam documentation on writing files](https://gleam.run/book/std/files.html#writing-files)
- [Gleam `io` library documentation](https://gleam.run/releases/v0.12.3/guide/io.html)