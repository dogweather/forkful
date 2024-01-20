---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists in Elixir is a way to verify the presence of a file system directory before performing operations on it. This is crucial to avoid errors while reading, writing, or navigating through directories.

## How to:
Checking for a directory in Elixir is straightforward thanks to the inbuilt `File.dir?/1` function:

```Elixir
directory = "/path/to/your/directory"
File.dir?(directory)
```

A sample output for a directory would read `true` or `false`, indicating whether the system recognized the path as an existing directory or not.

```Elixir
iex> directory = "/home/documents"
iex> File.dir?(directory)
true
```

## Deep Dive:
Historically, checking for the existence of a directory aided in error-proofing core tasks like file navigation, reading, and writing. As programming languages evolved, this basic function became fundamental, including in Elixir, which inherited the tradition from Erlang, its progenitor.

Alternative ways to check for a directory might include checking for errors when attempting to change into the directory with `File.cd/1`. However, using `File.dir?/1` is the preferred method due to its simplicity and readability.

Under the hood, Elixir's `File.dir?/1`, just like many file-related functions, relies on Erlang's `:file` module. It essentially wraps the `:file.read_file_info/1` function and checks if the `:type` field of the returned record is `:directory`.

## See Also:
Check out these useful resources that contribute further to this topic:

1. Elixir's official `File` module documentation: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
2. Erlang's `:file` module documentation: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)