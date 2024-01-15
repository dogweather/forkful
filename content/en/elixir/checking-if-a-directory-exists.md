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

## Why

Checking if a directory exists is a crucial part of file management in any programming language, Elixir included. In order to properly manipulate files and folders, it is important to verify that the desired directory exists before performing any operations on it.

## How To

To check if a directory exists in Elixir, we can use the `File.dir?/1` function. This function takes in a path to a directory as its argument and returns a boolean value indicating whether the directory exists or not.

Let's take a look at an example:

```Elixir
File.dir?("/home/user/my_directory")
```

The above code will return `true` if the "my_directory" folder exists in the "/home/user" directory, or `false` if it does not exist.

We can also use this function in combination with other file management functions, such as `File.list/1` which returns a list of all the files and folders within the specified directory.

```Elixir
if File.dir?("/home/user/my_directory") do
  files = File.list("/home/user/my_directory")
  # perform operations on the files in "my_directory"
end
```

## Deep Dive

Under the hood, the `File.dir?/1` function uses the `:filelib.is_dir/1` BIF (Built-In Function) provided by the Erlang standard library. This function checks whether the given path is a valid directory and returns a boolean. If the path does not exist or is not a directory, it will return `false`.

It is important to note that the `File.dir?/1` function only checks for the existence of the directory, not its accessibility or permissions. For further error handling, it is recommended to use the `File.readable?/1` or `File.writable?/1` functions to confirm if the directory can be read from or written to, respectively.

## See Also

- [File module Documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Standard Library Documentation](https://hexdocs.pm/elixir/stdlib/)

By following the steps outlined in this article, you should now have a better understanding of how to check if a directory exists in Elixir. Happy coding!