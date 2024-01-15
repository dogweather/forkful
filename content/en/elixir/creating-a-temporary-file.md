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

## Why

Creating temporary files can be useful for various reasons in programming. For example, temporary files can be used to store data during runtime, creating backups, or for testing purposes.

## How To

Creating a temporary file in Elixir is a simple process. First, we need to require the "Tempfile" module.

```Elixir
require Tempfile
```

Next, we can use the `Tempfile.open/1` function to create a new temporary file. This function takes in a prefix for the temporary file name.

```Elixir
temp_file = Tempfile.open("my_temp_file")
```

We can then write data to this file using the `write/1` function.

```Elixir
temp_file |> IO.write("This is a temporary file.")
```

To read from the temporary file, we can use the `read/1` function.

```Elixir
temp_file |> IO.read()
```

Once we are done using the temporary file, we can delete it using the `unlink/1` function.

```Elixir
Tempfile.unlink(temp_file)
```

## Deep Dive

When a temporary file is created, it is given a unique name and is stored in a temporary directory on the system. The location of this directory may vary depending on the operating system.

If we want to specify a different directory for the temporary file, we can use the `open/2` function instead of `open/1`. This function takes in a second argument which is the directory for the temporary file.

```Elixir
temp_file = Tempfile.open("my_temp_file", "/path/to/directory")
```

It is important to note that temporary files are automatically deleted when the process ends. However, if we want to delete the file before the process ends, we can use the `unlink/1` function as shown in the previous section.

## See Also

- [Documentation for Tempfile module](https://hexdocs.pm/elixir/Tempfile.html)
- [Elixir file IO tutorial on Temporary Files](https://elixir-lang.org/getting-started/file-operations.html#temporary-files)