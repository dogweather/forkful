---
title:    "Elixir recipe: Checking if a directory exists"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Why

As a developer, it is important to ensure that our code can handle all possibilities and scenarios. One of these possibilities is checking if a directory exists. This can help prevent unexpected errors and ensure smooth execution of our code.

# How To

To check if a directory exists in Elixir, we can use the `File.exists?/1` function. This function takes in a file or directory path as its argument and returns a boolean value indicating whether the path exists or not.

```
Elixir
defmodule Directory do
  def check_if_exists(path) do
    if File.exists?(path) do
      IO.puts "The directory exists"
    else
      IO.puts "The directory does not exist"
    end
  end
end

Directory.check_if_exists("project/lib") #=> The directory exists
Directory.check_if_exists("project/spec") #=> The directory does not exist
```

We can also use the `Dir.exists?/1` function to specifically check if a directory exists. This function works in the same way as `File.exists?/` but is optimized for working with directories.

```
Elixir
defmodule Directory do
  def check_if_exists(path) do
    if Dir.exists?(path) do
      IO.puts "The directory exists"
    else
      IO.puts "The directory does not exist"
    end
  end
end

Directory.check_if_exists("project/lib") #=> The directory exists
Directory.check_if_exists("project/spec") #=> The directory does not exist
```

# Deep Dive

The `File.exists?/1` and `Dir.exists?/1` functions use the Erlang library `:file` under the hood to perform a system call. This means that the functions will give accurate results regardless of the file system used (e.g. Windows, Unix).

It is worth noting that these functions do not check for file permissions. They only check if the file or directory exists. Therefore, it is important to handle errors that may arise due to insufficient permissions.

There is also the `File.regular?/1` function which can be used to specifically check if a given path is a regular file. This function will return `true` only if the path exists and is a regular file, otherwise it will return `false`.

# See Also

- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Dir module documentation](https://hexdocs.pm/elixir/Dir.html)