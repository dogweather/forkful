---
title:                "Checking if a directory exists"
html_title:           "Elixir recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a vital operation in file handling applications. Programmers do this to verify the existence of a directory before accessing or modifying its contents to prevent errors.

## How to:

In an Elixir program, you can verify if a directory exists by using the `File.dir?/1` function. Here's an example:

```Elixir
# Check if a directory exists
def check_directory(directory) do
  if File.dir?(directory) do
    IO.puts("#{directory} exists!")
  else
    IO.puts("#{directory} does not exists!")
  end
end

check_directory("/path/to/directory")
```

The function `File.dir?/1` returns a boolean indicating whether the provided path leads to a directory.



## Deep Dive

The `File.dir?/1` function is a part of the `File` module in Elixir, which provides functions to read, write, and manipulate files and directories. Introduced in the early versions of Elixir, this function is an essential aspect of the language's I/O operations. 

There are alternatives like checking for a directory by trying to open a directory with `File.ls/1` and seeing if it errors, but it's more involved and less explicit.

The `File.dir?/1` is a mere wrapper around the Erlang's `filelib` module's `is_directory/1` function. It emphasizes the interoperation between Elixir, built on Erlang's VM, with Erlang's extensive standard library.

## See Also

Details about the File module: https://hexdocs.pm/elixir/File.html  
Erlang's `filelib` documentation for more background: http://erlang.org/doc/man/filelib.html