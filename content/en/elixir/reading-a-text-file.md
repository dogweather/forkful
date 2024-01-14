---
title:                "Elixir recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why 
Reading and writing to text files is a common task in programming. In Elixir, there are easy and efficient ways to read and manipulate text files, making it a convenient language for tasks involving file handling.

## How To 
To read a text file in Elixir, we can use the `File.stream!` function. Let's say we have a text file named `data.txt` with the following content:

```
John
Jane
Harry
```

We can use the `File.stream!` function to open and read the file line by line, and then use `Enum.each` to iterate over each line and print it out. The code would look like this:

```
File.stream!("data.txt")
|> Enum.each(fn line -> IO.puts line end)
```

The output would be:
```
John
Jane
Harry
```

## Deep Dive 
Behind the scenes, the `File.stream!` function creates a stream of data, which allows us to lazily read and process the file. This means that the file is not loaded into memory all at once, making it a more efficient method for handling large files. Additionally, we can use the `File.stream!` function with a `:line` option to specify the number of lines we want to read at a time. This allows us to control the amount of memory used while reading the file.

We can also use the `File.read!` function to read the entire content of a file into memory at once. This can be useful for smaller files or when we need to perform operations on the entire file.

## See Also 
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Introduction to Elixir](https://elixir-lang.org/getting-started/introduction.html)