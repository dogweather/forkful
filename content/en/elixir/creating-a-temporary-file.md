---
title:    "Elixir recipe: Creating a temporary file"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

In programming, there are times when we need to create temporary files to store data temporarily. This can be useful for tasks such as caching, storing temporary data, or accessing files that are frequently updated. In Elixir, there are a few ways to create temporary files, and in this blog post, we will explore one method using the standard library.

## How To

To create a temporary file in Elixir, we will need to use the `:tempfile` module from the standard library. This module provides functions for creating and managing temporary files. Let's take a look at an example:

```
Elixir
{:ok, file} = :tempfile.open()
File.write(file, "This is a temporary file.")
File.close(file)
```

In this code snippet, we use the `:tempfile.open()` function to create a temporary file. The function returns a tuple with the status code `:ok` and the file descriptor `file`. We can then use the `File` module to write data to the file and close it when we are done.

Let's see what happens when we run this code:

```
Elixir
> {:ok, file} = :tempfile.open()
> {:ok, #PID<0.90.0>, #Reference<0.1225687213.944820767.2083>}
> File.write(file, "This is a temporary file.")
:ok
> File.close(file)
:ok
```

As we can see, the `:tempfile.open()` function returns a tuple with the status code `:ok`, the process ID of the temporary file, and a reference. We can use the process ID and reference to access the temporary file in our code.

## Deep Dive

Under the hood, the `:tempfile.open()` function uses the `:erlang.open\_\_temp\_\_0()` function which creates a temporary file in the system's temporary directory. The temporary file will have a unique name and will be deleted automatically when the process exits. We can also specify the directory and name of the temporary file by passing options to the `:tempfile.open()` function.

The `:tempfile.delete()` function can be used to manually delete a temporary file. If we want to access an existing temporary file, we can use the `:tempfile.open/2` function passing in the name and directory of the file as arguments.

## See Also

- [Elixir Tempfile Documentation](https://hexdocs.pm/elixir/Tempfile.html)
- [Erlang Tempfile Documentation](https://erlang.org/doc/man/file.html#open-2)