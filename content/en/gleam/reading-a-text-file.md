---
title:    "Gleam recipe: Reading a text file"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're new to Gleam programming, you may be wondering why it's important to learn how to read text files. Text files are a common data format used for storing information, and being able to read and manipulate them is a crucial skill for any programmer.

## How To

In Gleam, reading a text file is a simple process. First, you need to import the `gleam/io` module to access the necessary functions. Then, you can use the `read_text` function to read the contents of a file and store it in a variable. Let's take a look at an example:

```gleam
import gleam/io

file_contents = io.read_text("my_file.txt")
```

In the code above, we first import the `gleam/io` module. Then, we use the `read_text` function to read the contents of the file "my_file.txt" and store it in the variable `file_contents`. 

To print the contents of the file to the console, we can use the `print` function like this:

```gleam
print(file_contents)
```

This will output the contents of the file to the console. You can also use the `write_text` function to write to a text file. For more information on file I/O in Gleam, check out the official [documentation](https://gleam.run/documentation/#file-i-o).

## Deep Dive

When reading a text file, it's important to understand the different encoding formats that can be used. In Gleam, the `read_text` function supports reading files in different encodings such as UTF-8, UTF-16, and ASCII. This allows you to handle different types of text files with ease.

You can also use the `exists` function to check if a file exists before reading or writing to it. This can help prevent errors and ensure smooth execution of your program.

Another useful function is `read_lines`, which reads a text file line by line rather than as a single string. This can be helpful when working with large files, as it allows you to process the file one line at a time.

## See Also

For more information on Gleam file I/O, check out the following resources:

- [Official documentation](https://gleam.run/documentation/#file-i-o)
- [Gleam Cookbook](https://github.com/gleam-lang/gleam_cookbook/blob/main/input_output/file_io.gleam)
- [Learning Gleam](https://dev.to/zsolt_18/introduction-to-gleam-part-3-file-i-o-in-gleam-3aoo)

Now that you know how to read text files in Gleam, go forth and explore the possibilities of file I/O in your programs!