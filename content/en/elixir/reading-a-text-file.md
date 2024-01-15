---
title:                "Reading a text file"
html_title:           "Elixir recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file is a common task in any programming language, and Elixir makes it easy with its built-in `File` module. Whether you need to process a large dataset, read configuration files, or simply retrieve data from a text file, the `File` module has you covered. In this article, we will explore how to read a text file using Elixir.

## How To
To begin, we can use the `File.read!/1` function to read the contents of a text file into memory. This function takes in the path of the file as its argument and returns a string with the contents of the file.

```Elixir
file_contents = File.read!("data.txt")
IO.puts(file_contents)
```

In the above example, we use the `IO.puts/2` function to print the file contents to the console. You can also assign the output of `File.read!/1` to a variable for further processing.

Next, let's look at how to read a file line by line using the `File.stream!/1` function. This allows us to process large files without loading the entire contents into memory.

```Elixir
File.stream!("data.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

The `File.stream!/1` function returns a Stream, which we can then use with the `Enum.each/2` function to iterate through each line in the file and print it to the console. This is more memory efficient for large files, as it only reads and processes one line at a time.

## Deep Dive
By default, the `File.read!/1` function expects the file to be in UTF-8 encoding. However, if your file is in a different encoding, you can specify it as a second argument.

```Elixir
file_contents = File.read!("data.txt", [:encoding, "latin1"])
```

You can also use the `File.read!/2` function to specify other read options, such as `:cr`, `:unix`, or `:return` for line endings, and `:read_ahead` for buffering.

Additionally, the `File` module also provides functions such as `File.read_link!/1` for reading symbolic links, and `File.stat!/1` for retrieving file metadata.

## See Also
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir official website](https://elixir-lang.org/)
- [Elixir School - File I/O](https://elixirschool.com/en/lessons/advanced/files/)