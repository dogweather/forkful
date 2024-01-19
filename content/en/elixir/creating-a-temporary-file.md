---
title:                "Creating a temporary file"
html_title:           "Elixir recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file provides a storage spot for data over a brief period. Programmers do this to reduce memory usage and manage file interactions within programs efficiently. 

## How to
Elixir has a built-in module, ':file', for file manipulation tasks. This module can generate a temporary file for you:

```Elixir
{:ok, {path, device}} = :file.mkstemp("/tmp/tempfile")
IO.write(device, "Hello World")
```

In this code, the `:file.mkstemp` function creates a new temporary file in the "/tmp" directory with "tempfile" as the prefix. The function returns a tuple {:ok, {path, device}}, where path is the path to the temporary file, and the device is the IO device that represents the temporary file.

## Deep Dive
Elixir, although a modern language, has roots in the functional programming principles of the Erlang language and hence provides ample built-in functionality for file handling.

Creating a temporary file is not the only option. Often, you may require a temporary directory, not just a single file. Elixir's `:file` module has the function `:file.mkdtemp`, which creates a temporary directory:

```Elixir 
{:ok, path} = :file.mkdtemp("/tmp/tempdir")
```

Elixir's approach to file handling provides a high level of control over the lifecycle of the file. The creation of temporary files is handled by the operating system which ensures the file is unique, and it's the responsibility of your program to remove it when done.

## See Also
For more about the ':file' module and file manipulation in Elixir, take a look at these sources:
* [:file module documentation in Elixir](https://hexdocs.pm/elixir/File.html)
* [Working with files and directories in Elixir](http://elixir-recipes.github.io/files-directories/working-with-files-in-elixir/)
* [Elixir's Approach to Data and Disk Interaction](https://medium.com/the-erlang-rookie/elixir-s-approach-to-data-and-disk-interaction-96d8f1302a4d)