---
title:                "Reading a text file"
html_title:           "Elixir recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is the process of accessing and retrieving information stored in a file that contains plain text. This is a common task for programmers as it allows them to interact with external data and manipulate it within their code.

## How to:
A simple way to read a text file in Elixir is by using the ```File.read``` function:

```
Elixir: file = File.read("test.txt")
```

This code will read all the text from the "test.txt" file and save it into the ```file``` variable. If you want to read only a specific number of bytes, you can use the ```:cont``` option:

```
Elixir: file = File.read("test.txt", [:cont, 10])
```

In this example, the code will only read the first 10 bytes from the file. You can also specify the encoding of the file using the ```:encoding``` option, which defaults to UTF-8.

Another way to read a text file is by using the ```File.stream!``` function, which returns a stream that can be manipulated using different functions. For example:

```
Elixir: stream = File.stream!("test.txt") |> Enum.take(5)
```

This code will create a stream from the "test.txt" file and take the first 5 lines from it.

## Deep Dive:
In Elixir, the ```File.read``` and ```File.stream!``` functions use the underlying Erlang functions to read the file. This means that they are able to handle very large files efficiently, as they do not read the entire file into memory at once.

An alternative to using the built-in functions is to use the Elixir library ```NimbleCSV```, which allows you to read and manipulate CSV files in a more streamlined way. This can be useful if you need to work with structured data stored in a CSV file.

When reading a text file, Elixir uses the ```IO.get_line``` function, which reads one line at a time until the end of the file is reached. It also uses pattern matching to extract data from the file, which makes it easy to work with.

## See Also:
- [Elixir Documentation on File Module](https://hexdocs.pm/elixir/File.html)
- [NimbleCSV Library](https://hexdocs.pm/nimble_csv/readme.html)