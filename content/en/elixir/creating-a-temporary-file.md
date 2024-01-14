---
title:                "Elixir recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 

Creating temporary files is a common task in programming, and Elixir offers a straightforward and efficient way to handle this task. By creating temporary files, developers can store and manipulate data without having to create permanent files or disrupt existing files in their system. This can be especially useful for tasks like file downloads, caching data, or testing purposes.

## How To

To create a temporary file in Elixir, we can use the `File.tempfile/2` function. This function takes in two arguments: a name for the temporary file and the directory where the file will be created. Let's look at an example of how we can use this function:

```
Elixir

{:ok, file} = File.tempfile("my_temp_file", "/tmp")
```

In this example, we are creating a temporary file named "my_temp_file" in the "/tmp" directory. The result of this function is a tuple with the first element being `:ok` and the second element being the file path of the created temporary file.

To write data to the temporary file, we can use the `File.write/2` function:

```
Elixir

File.write(file, "This is a temporary file.")
```

This will write the string "This is a temporary file." to our temporary file. We can then use the functions in the `File` module to read and manipulate data in our temporary file as needed.

Once we are finished using the temporary file, we can delete it using the `File.rm/2` function:

```
Elixir

File.rm(file)
```

This will permanently delete the temporary file from our system.

## Deep Dive 

Behind the scenes, the `File.tempfile/2` function creates a unique name for the temporary file by appending a random string to the given file name. This ensures that each time we create a temporary file, it will have a unique name and will not overwrite any existing files.

It is also worth noting that we can specify a different directory for our temporary file if needed. The `File.tempfile/2` function will check if the given directory exists and has proper permissions before creating the temporary file.

## See Also 

- Official Elixir Documentation for `File` module: https://hexdocs.pm/elixir/File.html
- Elixir School's tutorial on working with temporary files: https://elixirschool.com/en/lessons/basics/files/#temporary-files
- Phoenix Framework's blog post on creating temporary files in Elixir: https://phoenixframework.org/blog/serving-files-with-plug