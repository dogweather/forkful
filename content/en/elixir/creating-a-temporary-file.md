---
title:                "Elixir recipe: Creating a temporary file"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Creating temporary files is a common practice in programming to store and manipulate data that is only needed for a short period of time. It allows for efficient use of system resources and can also provide security benefits.

## How To
To create a temporary file in Elixir, we can use the `Tempfile` module from the standard library. This module provides functions for creating, reading, and writing temporary files.

Let's look at an example of creating a temporary file and writing data to it:

```Elixir
temp_file = Tempfile.open("my_file")
Tempfile.write(temp_file, "Hello World")
```

In this code, we first open a new temporary file using the `Tempfile.open/1` function and give it a name of "my_file". Then, we use the `Tempfile.write/2` function to write the string "Hello World" to the file. Both these functions return a tuple containing the temporary file and its path.

We can also specify the directory where we want the temporary file to be created:

```Elixir
temp_file = Tempfile.open("my_file", "my_directory")
```

This will create the temporary file in the `my_directory` folder instead of the system's default temporary folder.

To read data from a temporary file, we can use the `Tempfile.read/1` function:

```Elixir
Tempfile.read(temp_file)
#=> "Hello World"
```

Once we are done using the temporary file, we can delete it by calling the `Tempfile.close/1` function:

```Elixir
Tempfile.close(temp_file)
```

## Deep Dive
Behind the scenes, the `Tempfile` module uses the `:tempfile` Erlang module to create temporary files. This module creates the temporary file in a secure manner and keeps it open until it is explicitly closed, ensuring its contents are not accessible to other processes.

The `Tempfile` module also takes care of cleaning up temporary files when the process exits or crashes, preventing them from cluttering the system. However, it's important to note that the files will not be deleted if the process is forcefully terminated.

## See Also
- Elixir `Tempfile` module documentation: https://hexdocs.pm/elixir/Tempfile.html
- Erlang `:tempfile` module documentation: http://erlang.org/doc/man/tempfile.html