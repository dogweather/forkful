---
title:    "Elixir recipe: Reading a text file"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Why

Reading a text file is a crucial task for any programmer, as it allows for the retrieval and manipulation of data in a structured format. In Elixir, reading text files can be done efficiently and effectively, making it an essential skill for any developer.

# How To

To read a text file in Elixir, we can use the `File.read!` function. This function takes in the file path as its argument and returns the contents of the file as a binary string. Let's see an example:

```
Elixir

file_contents = File.read!("text_file.txt")
IO.puts file_contents
```

In the above code, we are using the `File.read!` function to read the contents of a text file called "text_file.txt". We are then using `IO.puts` to print out the contents to the console.

We can also use the `File.stream!` function to read larger files, as it returns an enumerable stream instead of loading the entire file into memory. Here's an example of how we can use it:

```
Elixir

file_stream = File.stream!("big_text_file.txt")
Enum.each(file_stream, &IO.puts/1)
```

In this code, we are using the `File.stream!` function to stream the contents of a large text file called "big_text_file.txt". We are then using the `Enum.each` function to iterate over the stream and print out each line using the `IO.puts` function.

# Deep Dive

When reading a text file, it's essential to consider the encoding of the file. By default, `File.read!` and `File.stream!` functions assume that the file is encoded in UTF-8, but we can specify a different encoding if needed. For example, to read a file encoded in Latin-1, we can use the `:latin1` option like this:

```
Elixir

file_contents = File.read!("text_file.txt", [:encoding, :latin1])
```

Additionally, we can also use the `File.open` function to open a file and manipulate its contents using the `IO.write` or `IO.binwrite` functions.

# See Also

- Elixir Documentation on File Module: https://hexdocs.pm/elixir/File.html
- Reading and Writing Files in Elixir from DigitalOcean: https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-elixir
- Elixir File Operations Tutorial: https://www.tutorialspoint.com/elixir-programming/elixir_file_operations.htm