---
title:                "Elixir recipe: Reading a text file"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file may seem like a mundane task, but in the world of programming, it is a crucial skill to have. With Elixir's built-in functions, it becomes even easier to manipulate and extract information from text files. In this blog post, we will explore how to do just that using Elixir.

## How To

Reading a text file in Elixir is quite simple. Let's take a look at a basic example of reading a file and printing its contents:

```elixir
file_path = "example.txt"
file = File.open(file_path)

IO.puts(File.read(file))

File.close(file)
```

Here, we first define the file path, then open the file using `File.open()`, and finally print its contents using `File.read()`. It is important to close the file using `File.close()` once we are done to avoid any memory leaks. 

We can also specify the mode in which we want to open the file, such as read, write, or append. For example, to read a file in binary mode, we can use the following code:

```elixir
file_path = "binary.txt"
file = File.open(file_path, [:binary])

IO.puts(File.read(file))

File.close(file)
```

This will read the file as binary data instead of regular text. We can also use other modes such as `:utf8` or `:ascii` depending on the type of data in our text file.

## Deep Dive

There are various Elixir functions that can be used for reading text files, such as `File.read()`, `IO.read()`, and `IO.gets()`. Each of these functions has its own use case and offers different features such as reading a specific number of bytes or lines from a file.

Elixir also has a `File.stream!()` function, which allows us to read a file line by line without loading the entire file into memory. This is useful for large text files, as it helps to save memory and increase performance.

It is also worth noting that Elixir allows us to pattern match on file data. This means that we can extract specific information from a text file by matching on a specific pattern, making it easier to process and manipulate data.

## See Also

- Elixir File Module Documentation: https://hexdocs.pm/elixir/File.html
- Elixir IO Module Documentation: https://hexdocs.pm/elixir/IO.html
- Elixir File Streams: https://learningelixirjargon.wordpress.com/elixir-file-streams/

Happy coding!