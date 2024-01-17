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
Checking if a directory exists is a way for programmers to determine if a given directory is present in a specified path. This is commonly used in file management and error handling, as it allows developers to handle directories and files accordingly.

## How to:
```Elixir
# Using the File module
File.dir?("path/to/directory")

# Using the Path module
Path.dir?("path/to/directory")
```

Sample Output:
`true` - if the directory exists
`false` - if the directory does not exist

## Deep Dive:
The functionality of checking if a directory exists is not specific to Elixir. The concept of checking for existence of a directory has been present in many programming languages, such as Java and Python, for years. In Elixir, there are two common ways to check for directory existence: by using the File module or the Path module.

The `dir?` function in the File module takes a path as input and returns a boolean value indicating whether a directory exists at that path or not. The same functionality can also be achieved by using the `dir?` function in the Path module, which is specifically designed to handle file paths.

In terms of alternatives, there are other methods to check for directory existence, such as using Unix commands through the `:os.cmd` function or using the Erlang `:filelib` library. However, the File and Path modules are the recommended ways in Elixir to handle file management tasks.

## See Also:
Elixir official documentation for the [File](https://hexdocs.pm/elixir/File.html) and [Path](https://hexdocs.pm/elixir/Path.html) modules.