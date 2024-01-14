---
title:    "Elixir recipe: Reading a text file"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're a beginner in Elixir or a seasoned developer looking to expand your skills, reading a text file is an essential task that you may encounter in your programming journey. In this blog post, we will explore how to read a text file in Elixir and the various options available to accomplish this task. 

## How To

Reading a text file in Elixir is a straightforward process. We can use the `File.read` function to read the file and store its contents in a variable. Let's take a look at an example:

```
Elixir
# Reading a text file
file_contents = File.read("example.txt")

# Printing the file contents to the console 
IO.puts(file_contents)
```
If we have the following content in our `example.txt` file:
```
Hello, world!
This is a text file.
```
The output of our code will be:
```
Hello, world!
This is a text file.
```

We can also use the `File.stream!` function to read large files efficiently. This function returns an enumerator, and we can use the `Enum.each` function to iterate over the lines of the file. Let's see an example:

```
Elixir
# Reading a large text file
file_lines = File.stream!("big_file.txt")

# Printing each line to the console
Enum.each(file_lines, fn line ->
  IO.puts(line)
end)
```

## Deep Dive

Elixir also offers different options to handle different types of files. For example, we can use `CSV.decode` to read CSV files or `Jason.decode` to read JSON files. We can also specify the format for reading a file using the `:encoding` option in the `File.read` function. Elixir also has built-in functions to handle reading binary files and to skip or rewind a file while reading.

Additionally, Elixir has a special function called `IO.gets` that can be used to read a file line by line. This function takes in a file handle and returns the next line in the file. It also has options to specify the delimiter and whether to include the delimiter in the returned string.

It's also important to note that Elixir's `File` module offers error handling functions, such as `File.read!` and `File.stream!`. These functions raise exceptions if there is an error while reading the file, making error handling easier for developers.

## See Also

To learn more about reading files in Elixir, check out these resources:

- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Enum module documentation](https://hexdocs.pm/elixir/Enum.html)
- [IO module documentation](https://hexdocs.pm/elixir/IO.html)

Happy coding!