---
date: 2024-01-20 17:53:52.551148-07:00
description: "Reading a text file means pulling data from a file into your program.\
  \ Programmers do this to process or analyze the content, like reading configs,\u2026"
lastmod: '2024-03-13T22:44:59.798046-06:00'
model: gpt-4-1106-preview
summary: "Reading a text file means pulling data from a file into your program. Programmers\
  \ do this to process or analyze the content, like reading configs,\u2026"
title: Reading a text file
weight: 22
---

## What & Why?

Reading a text file means pulling data from a file into your program. Programmers do this to process or analyze the content, like reading configs, analyzing logs, or importing data.

## How to:

Here's how to read the entire contents of a text file named `example.txt`:

```elixir
File.read("example.txt")
```

Sample output if `example.txt` contains "Hello, Elixir!":

```elixir
{:ok, "Hello, Elixir!"}
```

To read the file line by line:

```elixir
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

This will print each line of `example.txt` to the console.

## Deep Dive

In Elixir, `File.read/1` and `File.stream!/1` are typical ways to read text files. Historically, file reading in programming originates from the need to store and retrieve data. In early computing, this was done using punch cards or magnetic tapes. Today, we use various storage devices like SSDs, HDDs, and more.

An alternative to `File.read/1` is `File.read!/1`, which raises an error if something goes wrong instead of returning a tuple. Similarly, `File.stream!/1` differs from `File.stream/1` by raising an error on failure rather than returning an error tuple.

The implementation under the hood deals with binary data. Text is converted into binaries by Elixir, which handles the underlying bytes and encoding.

## See Also:

- Elixir's official `File` module documentation: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
