---
date: 2024-02-03 19:03:17.004048-07:00
description: "How to: Elixir makes file handling straightforward with built-in modules.\
  \ The primary way to write to a file is using the `File.write/2` or\u2026"
lastmod: '2024-03-13T22:44:59.798867-06:00'
model: gpt-4-0125-preview
summary: Elixir makes file handling straightforward with built-in modules.
title: Writing a text file
weight: 24
---

## How to:
Elixir makes file handling straightforward with built-in modules. The primary way to write to a file is using the `File.write/2` or `File.write!/2` functions, where the former returns an `:ok` or `:error` tuple and the latter raises an error on failure. 

Here's a simple example:

```elixir
# Writing to a file, simple message
File.write("hello.txt", "Hello, World!")

# When you run the code, it creates 'hello.txt' with "Hello, World!" as content
```

For appending to files, you'd use `File.open/3` with the `[:write, :append]` options, then `IO.binwrite/2` to append the content:

```elixir
# Appending to a file
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nLet's add another line.")
File.close(file)

# Now 'hello.txt' includes a second line "Let's add another line."
```

If you're working with large data or need more control over the writing process, you might use the `Stream` module to lazily write data to the file:

```elixir
# Writing a large dataset lazily
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Number: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# This creates 'numbers.txt', writing numbers 0 to 9, each on a new line.
```

For projects that require more sophisticated file handling, you might look into third-party libraries like `CSV`, which offers tailored functionalities for CSV file manipulation but remember, for many purposes, Elixir's built-in capabilities are more than sufficient.
