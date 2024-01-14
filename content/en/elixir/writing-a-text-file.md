---
title:    "Elixir recipe: Writing a text file"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Writing text files may seem like a mundane task, but it is an integral part of programming. It allows us to store and manipulate data, making our applications more dynamic and versatile.

## How To
To write a text file in Elixir, we will be using the File module's `write/2` function. This function takes in two arguments: the file path and the content that we want to write.

```
Elixir
file_path = "my_file.txt"
file_content = "Hello Elixir!"
File.write(file_path, file_content)
```

If we want to append to an existing file, we can use the `write/3` function and provide `:append` as the third argument.

```
Elixir
File.write(file_path, " New content!", [:append])
```

The content we pass into the `write` functions can be of any data type, as Elixir will automatically convert it to a string. We can also use interpolation with strings to insert variables into our text files.

```
Elixir
name = "John"
age = 25
File.write(file_path, "My name is #{name}, and I am #{age} years old!")
```

The above code will result in the file containing the following text: "My name is John, and I am 25 years old!"

When we run the code above, we don't see any output in the terminal. To verify that our file was indeed written, we can use the `File.read/1` function to read the file's contents.

```
Elixir
File.read(file_path)
```

This function will return the contents of our file as a binary. If we want to convert it to a string, we can use the `to_string/1` function.

```
Elixir
File.read(file_path) |> to_string()
```

## Deep Dive
When writing a text file, it is essential to keep a few things in mind. Firstly, it is good practice to always check for errors when using the `write` functions. This can be done using the `case` statement.

```
Elixir
case File.write(file_path, file_content) do
  :ok -> IO.puts("File successfully written.")
  {:error, reason} -> IO.puts("Error: #{reason}.")
end
```

We can also specify the file encoding when writing text files by passing it as a third argument in the `write/3` function.

```
Elixir
File.write(file_path, file_content, [:utf8])
```

Lastly, it is crucial to close the file after writing to it to ensure that all the contents are saved. We can use the `File.close/1` function to do so.

```
Elixir
File.close(file_path)
```

## See Also
- Official Elixir documentation for the File module: https://hexdocs.pm/elixir/File.html

- Tutorial to learn more about writing and reading files in Elixir: https://elixirschool.com/en/lessons/basics/file/

- Nice blog post explaining the different ways to write files in Elixir: https://henriquefernandes.com.br/elixir-how-to-write-a-file