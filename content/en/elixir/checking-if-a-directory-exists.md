---
title:                "Elixir recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When working with files and directories in a program, it is important to ensure that the desired directory exists before performing any actions on it. In Elixir, this can be done using the `File.dir?/1` method.

## How To

To check if a directory exists in Elixir, simply pass in the path of the directory as an argument to the `File.dir?/1` method. For example:

```Elixir 
File.dir?("/home/user/Documents") 
```

The output of this code will be either `true` or `false`, depending on whether the directory exists or not. Let's see an example:

```Elixir
File.dir?("/home/user/Downloads")
```

If the directory "Downloads" exists in the "/home/user" path, the output will be `true`. Otherwise, if the directory does not exist, the output will be `false`.

## Deep Dive

The `File.dir?/1` method uses the underlying operating system's API to check for the existence of a directory. This means that it is platform-specific and may behave differently on different operating systems.

Another thing to note is that this method only checks for the existence of the directory, it does not differentiate between directories and regular files. So if a regular file exists with the same name as the directory, the method will return `true` even though it is not a directory.

To check if a directory is empty, the `File.ls/1` method can be used in conjunction with the `File.dir?/1` method. The `File.ls/1` method returns a list of files in the given directory, so an empty list would indicate an empty directory.

## See Also

- [Elixir Documentation on File](https://hexdocs.pm/elixir/File.html)
- [Elixir File Module cheatsheet](https://devhints.io/elixir-file)