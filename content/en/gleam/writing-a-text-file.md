---
title:    "Gleam recipe: Writing a text file"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Are you new to Gleam programming and wondering why you would need to know how to write a text file? Writing text files is a fundamental skill in programming and can be used for a variety of tasks such as creating configuration files, saving user data, or generating reports.

## How To

To write a text file in Gleam, you first need to import the `gleam/io` module which provides the `File` type and its associated functions. Next, you can use the `File.write` function to write your desired text to the file. Here's an example:

```Gleam
import gleam/io

let file = File.from_path("./example.txt")
File.write(file, "Hello World!")
```

This will create a file named "example.txt" in the current directory and write the text "Hello World!" to it. If you want to write multiple lines of text, you can use the `File.puts` function, which automatically adds a new line after each call. For example:

```Gleam
import gleam/io

let file = File.from_path("./example.txt")
File.puts(file, "Hello")
File.puts(file, "World!")
```

The resulting file will have the following content:

```
Hello
World!
```

You can also use string interpolation to create dynamic content in your text file. For instance:

```Gleam
import gleam/io

let name = "John"
let age = 30

let file = Files.from_path("./user.txt")
File.write(file, "Name: {name}\nAge: {age}")
```

This will generate a file with the following content:

```
Name: John
Age: 30
```

## Deep Dive

For more advanced usage, the `File` type also provides other functions such as `write_all` to write a list of bytes to a file, and `zap` to delete a file. You can also specify the mode of the file when creating it using the `File.with_mode` function, which allows you to set permissions for the file.

Additionally, Gleam also has the `gleam/binary` module which provides functions for encoding and decoding data to be written to or read from a text file.

## See Also

- [Official Gleam documentation on writing text files](https://gleam.run/book/tutorials/writing_text_files.html)
- [Gleam IO module documentation](https://gleam.run/modules/gleam-io/latest/File.html)
- [Gleam Binary module documentation](https://gleam.run/modules/gleam-binary/latest/Binary.html)