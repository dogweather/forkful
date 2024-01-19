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

# Reading a Text File in Elixir (Version 1.12) 

## What & Why?

Reading a text file means fetching data from a file and loading it into your program. It's fundamental for programmers: often, we need to extract data stored in files (logs, datasets etc.), process it and generate results.

## How to:

Reading a file in Elixir can be done in a few lines of code. Here's an example using the Elixir's built-in function `File.read/1`:

```Elixir
{:ok, content} = File.read("yourfile.txt")
IO.puts content
```

That would display the contents of `yourfile.txt` to your console. If an error occurs, it's typically because the file is missing or the program doesn't have proper permissions to access it.

On a side note, `File.read/1` reads the entire file into memory. It's handy but can be inefficient for large files. Elixir offers another function, `File.stream!/1`, that returns a stream to read the file line by line:

```Elixir
File.stream!("yourfile.txt")
|> Enum.each(&IO.puts(&1))
```
Each line is processed as it's read from the file, which makes it a good choice for memory management with large files.

## Deep Dive

Elixir's file reading functions draw from its ancestor, Erlang, and have been developed to handle concurrency and fault-tolerance well.

Aside from `File.read/1` and `File.stream!/1`, there are a few other alternatives. `File.read!/1` and `File.read_file/1` are similar but handle errors differently. Which to use depends on how you want your program to respond to errors. 

Implementation-wise, these functions wrap around the lower-level `:file.open/2` and `:file.read/2` Erlang functions, which you could use directly. However, it's recommended to stick with the Elixir's `File` module functions, as they incorporate many handy features, such as automatic file closing and error handling.

## See Also

For more information and details, check out the official Elixir documentation:

- [File Module](https://hexdocs.pm/elixir/File.html)
- [File.read/1](https://hexdocs.pm/elixir/File.html#read/1)
- [File.stream!/1](https://hexdocs.pm/elixir/File.html#stream!/1)

Remember, code is just a tool. The real power in programming lies in understanding what you're doing and why. Learn from the documentation, experiment on your own, and keep refining your craft.