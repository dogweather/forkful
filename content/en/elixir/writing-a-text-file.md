---
title:                "Writing a text file"
html_title:           "Elixir recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming, and it serves multiple purposes such as storing data, creating configurations, and generating reports. In Elixir, writing a text file can be easily accomplished with the help of built-in modules and functions.

## How To

To write a text file in Elixir, we can use the `File.write/3` function. It takes three arguments: the file path, the data to be written, and the mode in which the file should be opened.

```Elixir
filename = "sample.txt"
data = "This is a sample text file."
File.write(filename, data, [:write])
```

In the above example, we have created a new file called "sample.txt" and written the text "This is a sample text file" to it. The file mode `[:write]` indicates that if the file already exists, its contents will be overwritten. If we want to append data to an existing file, we can use the mode `[:append]` instead.

We can also use the `IO.write/2` function to write data to a file without explicitly opening it. This function takes two arguments: the data to be written and the file path.

```Elixir
data = "This data will be written to sample.txt."
IO.write(data, "sample.txt")
```

The above code will append the data to the end of the file if it already exists. To overwrite the existing file, we can use the `IO.stream!/3` function which takes three arguments: the data to be written, the mode, and the file path. 

```Elixir
data = "This data will overwrite the existing file."
IO.stream!(data, [:write], "sample.txt")
```

## Deep Dive

The `File.write/3` function uses the `File` module which provides various functions for working with files. It is important to note that the file path can be either an absolute path or a relative path, and if a relative path is given, it will be relative to the current working directory.

To create nested directories and files, we can use the `Path.expand/2` function for constructing the file path and the `File.mkdir_p/1` function for creating directories recursively.

```Elixir
path = Path.expand(["data", "reports.txt"], ".")
File.mkdir_p(Path.dirname(path)) # creates the "data" directory if it doesn't exist
File.write(path, "This is my report.")
```

Besides writing data to a file, we can also use the `File.copy/2` function to copy a file or the `File.rename/2` function to rename a file.

## See Also

* Official Elixir documentation for File module - https://hexdocs.pm/elixir/File.html
* ElixirSchool tutorial on working with files - https://elixirschool.com/en/lessons/advanced/files/
* ElixirForum thread on writing to a file - https://elixirforum.com/t/write-to-a-file-in-elixir/2645/2