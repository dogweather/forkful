---
title:    "Elixir recipe: Writing a text file"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why Writing a Text File in Elixir is Useful

Writing a text file may seem like a mundane task, but it can be extremely useful in certain situations. For example, if you're working on a project that requires storing or manipulating large amounts of data, writing to a text file can be a convenient and efficient solution. Additionally, text files can be easily shared and read by other programs, making them a popular choice for data storage.

## How To Write a Text File in Elixir

To write a text file in Elixir, we will use the `File.write` function. Let's say we want to create a file called "data.txt" and write the string "Hello, world!" to it. Here's the code:

```elixir
File.write("data.txt", "Hello, world!")
```

This will create a new file named "data.txt" in the current working directory and write the specified string to it. If the file already exists, it will be overwritten.

We can also write multiple lines of text to a file by using the `IO.write` function in a loop. For example, let's write numbers 1-10 to a file called "numbers.txt":

```elixir
File.open("numbers.txt", [:write], fn(file) ->
  for n <- 1..10 do
    IO.write(file, "#{n}\n")
  end
end)
```

This code will open the file for writing, then use `IO.write` to write each number on its own line. After we're done writing, we need to close the file using `File.close` to ensure that all data is written to the file. 

## Deep Dive into Writing a Text File

There are a few different options for writing to a text file in Elixir, depending on your specific needs. One useful function is `IO.fwrite`, which allows us to use string interpolation to write variables to a file. For example:

```elixir
name = "John"
age = 25
IO.fwrite("My name is ~s and I am ~B years old.", [name, age])
```

This will write the string "My name is John and I am 25 years old." to the file.

Another thing to keep in mind when writing text files in Elixir is that some characters may need to be escaped. For example, if our string contains quotation marks or backslashes, we will need to add a backslash before them to ensure they are written correctly. The `String.replace` function can be helpful in this situation.

## See Also

- [File.write documentation](https://hexdocs.pm/elixir/File.html#write/2)
- [IO.write documentation](https://hexdocs.pm/elixir/IO.html#write/2)
- [String.replace documentation](https://hexdocs.pm/elixir/String.html#replace/4)