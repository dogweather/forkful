---
title:                "Elixir recipe: Checking if a directory exists"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an important aspect of programming, especially in file management tasks. It allows for error handling and ensures that your code is robust and able to handle various scenarios.

## How To

To check if a directory exists in Elixir, we can use the `File.exists?` function. This function returns a boolean value of `true` or `false` depending on whether the directory exists or not.

Let's take a look at an example:

```Elixir
directory = "/path/to/directory"

if File.exists?(directory) do
  IO.puts("The directory exists!")
else
  IO.puts("The directory does not exist.")
end
```

If the directory exists, the output will be: `The directory exists!` Otherwise, the output will be: `The directory does not exist.`

We can also use the `File.dir?` function to specifically check if a given path is a directory. It also returns a boolean value.

```Elixir
path = "/path/to/file.txt"

if File.dir?(path) do
  IO.puts("The path is a directory.")
else
  IO.puts("The path is not a directory.")
end
```

In this case, if the path is a directory, the output will be: `The path is a directory.` Otherwise, the output will be: `The path is not a directory.`

## Deep Dive

Behind the scenes, the `File.exists?` and `File.dir?` functions use the Erlang functions `:file.readable?` and `:file.is_dir?` respectively. These functions make system calls to the underlying OS to determine the existence and type of a given path.

It is important to note that the `File.exists?` function may return a false negative if the path is a broken or inaccessible symbolic link. In this case, `File.exists?` will return `false` even if the directory does actually exist.

## See Also

- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html)
- [Erlang File Module Documentation](https://erlang.org/doc/man/file.html)
- [Check if a Directory Exists in Elixir (Gist)](https://gist.github.com/jasongarrett/4c6cdcbfc7c6e6041f45d6a5cca7b6c6)