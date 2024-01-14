---
title:    "Elixir recipe: Creating a temporary file"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Creating a temporary file is a common task in programming, especially when dealing with large amounts of data or when we need to store information temporarily before performing a specific action on it. In Elixir, we can easily create and manipulate temporary files, making our coding experience smoother and more efficient.

## How To 
Creating a temporary file in Elixir is a straightforward process. We will use the `File` module and its `temp_file` function to generate a unique file name and create the temporary file.

```Elixir
{:ok, path} = File.temp_file()
IO.puts("Temporary file created at path: #{path}")
```
This will create a temporary file with a random name and return its path. We can also specify a specific directory and prefix for the file name as arguments to the `temp_file` function.

```Elixir
{:ok, path} = File.temp_file("/my_directory", "my_prefix")
IO.puts("Temporary file created at path: #{path}")
```
The above code will create a temporary file with the name "my_prefix" in the `my_directory` directory.

Once we have the path to our temporary file, we can use the `File.write` function to write data into it, and `File.read` to read the data from the file.

```Elixir
{:ok, path} = File.temp_file("my_data.txt")
File.write(path, "Hello world!")
IO.puts("Data written to temporary file.")
data = File.read(path)
IO.puts(data) # Output: "Hello world!"
```

## Deep Dive 
When we create a temporary file using `File.temp_file`, it uses the default `:prefix` and `:suffix` options for generating the file name. We can also pass in custom options to the `temp_file` function, such as setting a specific file extension.

```Elixir
{:ok, path} = File.temp_file("my_data", "", [suffix: ".csv"])
IO.puts("Temporary file created with extension: #{Path.extname(path)}") # Output: .csv
```
We can also specify the `:directory` option to create the temporary file in a specific directory, and the `:mode` option to set the permissions for the file.

See the [official documentation](https://hexdocs.pm/elixir/File.html#temp_file/3) for a full list of options and their usage.

## See Also 
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Path module documentation](https://hexdocs.pm/elixir/Path.html)
- [Elixir IO module documentation](https://hexdocs.pm/elixir/IO.html)