---
title:                "Elixir recipe: Writing a text file"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is an essential skill for any programmer, as it allows you to store and manipulate data in a readable format. In the world of Elixir programming, knowing how to write a text file is especially important as it gives you the ability to exchange data with other applications or systems.

## How To

To write a text file in Elixir, we will use the `File.write` function. This function takes in two parameters: the name of the file and the data you want to write. Let's see an example of how to use this function to write the name "John" to a file called "name.txt".

```Elixir
File.write("name.txt", "John")
```

This will create a file called "name.txt" in the same directory as your Elixir code and write the data "John" to it. If the file already exists, the `File.write` function will overwrite its contents.

We can also write more complex data to a text file by using Elixir's interpolation feature. Let's say we have a list of names and we want to write each one on a new line in a text file called "names.txt":

```Elixir
names = ["John", "Alice", "Mark"]
File.write("names.txt", for name <- names, do: "#{name}\n")
```

This will create a file called "names.txt" and write the names on separate lines, like this:

```
John
Alice
Mark
```

## Deep Dive

When writing a text file in Elixir, it's essential to understand how the underlying file system functions. By default, Elixir uses the same encoding as the underlying file system, which is typically UTF-8.

You can also specify the encoding when writing a file by passing the `:encoding` option as a third parameter to the `File.write` function. For example, if you want to save the file in UTF-16 encoding, you can do so by writing:

```Elixir
File.write("names.txt", "John", encoding: :utf16)
```

Additionally, you can also use the `:append` option to append data to an existing file instead of overwriting its contents. This can be useful for creating log files that continuously add new entries.

## See Also

Check out these resources for further reading on writing text files in Elixir:

- Official Elixir documentation for `File.write`: https://hexdocs.pm/elixir/File.html#write/2
- Blog post on file manipulation in Elixir: https://medium.com/@jtmccormick/working-with-files-in-elixir-741b4af02ae1
- Elixir Forum discussion on writing text files: https://elixirforum.com/t/write-text-to-file/1381