---
title:                "Writing a text file"
html_title:           "Elixir recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file means creating a digital document that contains plain text, such as letters, numbers, or symbols. Programmers use text files to store data, write code, and configure applications because they are easy to read and manipulate.

## How to:

Writing a text file in Elixir is a simple and straightforward process. First, we need to import the `File` module which provides functions for working with files. Then, we can use the `write` function to create a new text file and write some content into it.

```elixir
import File
write("my_text_file.txt", "Hello, world!")
```

This will create a new text file named `my_text_file.txt` and write the text "Hello, world!" into it. We can also use the `write!` function to append the content to an existing file, or the `mkdir` function to create a new directory.

```elixir
write!("my_text_file.txt", "New line added!")
mkdir("my_directory")
```

We can also use the `append` function to add content to an existing text file without overwriting it, or the `del_dir` function to delete a directory. Here's an example of appending to a file:

```elixir
append("my_text_file.txt", "Another line!")
```

To read the content of a text file, we can use the `read` function. This will return the content of the file as a string.

```elixir
read("my_text_file.txt")
# => "Hello, world!\nNew line added!\nAnother line!"
```

## Deep Dive

Text files have been around since the earliest days of computing and are still widely used today. They are versatile and can be opened and edited by a variety of programs and operating systems. Alternatives to writing text files include using databases or JSON files, but text files remain a popular choice due to their simplicity and portability.

Writing a text file in Elixir uses a combination of the `File` and `IO` modules. The `write` and `write!` functions internally call the `IO.puts` function, which writes the content to the specified file. This makes use of the underlying operating system's file handling capabilities. Similarly, the `read` function internally uses the `IO.binread` function to read the content of the file as a binary, and then returns it as a string.

## See Also

- [Elixir Documentation on the File module](https://hexdocs.pm/elixir/File.html)
- [Elixir Documentation on the IO module](https://hexdocs.pm/elixir/IO.html)