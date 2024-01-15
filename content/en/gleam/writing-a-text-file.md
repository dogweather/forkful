---
title:                "Writing a text file"
html_title:           "Gleam recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is an essential task for any developer. It allows you to easily store and transfer data, making it a crucial part of many programming projects.

## How To

To write a text file in Gleam, the first step is to import the `gleam/io` module. This will give us access to the necessary functions. Here's an example of how you can create and write to a file:

```
import gleam/io

fn main() {
  let file = gleam/io.file("my_file.txt")
  write_to_file(file, "Hello, world!")
}
```

In the above code, we first create a file named "my_file.txt" and then write the string "Hello, world!" to it using the `write_to_file` function. This function takes in the file we created and the data we want to write as arguments.

You can also use Gleam's built-in string interpolation to easily write data to a file. Here's an example:

```
import gleam/io

fn main() {
  let name = "John"
  let file = gleam/io.file("hello.txt")
  write_to_file(file, "Hello, {name}!")
}
```

In the above code, we use the `write_to_file` function to write "Hello, John!" to the file "hello.txt". As you can see, using string interpolation makes it much easier to write dynamic data to a file.

## Deep Dive

When writing a text file in Gleam, you have the option to specify the encoding of the file using the `Encoding` type. This is useful if you want to support different character sets, such as UTF-8 or ASCII. By default, Gleam will use UTF-8 encoding.

You can also specify the file mode when creating a file using the `Mode` type. This allows you to control whether the file is opened for reading, writing, or both. By default, Gleam will open the file for reading and writing.

Lastly, you can use the `write_line_to_file` function to write data to a file and automatically add a new line at the end. This is useful if you want to write data to a file in a structured format, such as CSV.

## See Also

- [Gleam documentation on writing files](https://gleam.run/book/tour/files.html)
- [Official Gleam website](https://gleam.run/)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)