---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file is a process where a program retrieves and interprets data from a .txt or similar file. Programmers do it to manipulate, analyze, or display the content according to the app's functionality.

## How to:
Reading a text file in Elixir is straightforward. Here, we're using the `File.read/1` function.

```Elixir
{:ok, content} = File.read("my_text_file.txt")
IO.puts(content)
```

If `my_text_file.txt` has the sentence "Hello, Elixir!" this code will output:

```
Hello, Elixir!
```

If there's an error, Elixir uses "pattern matching" to handle it efficiently.

```Elixir
case File.read("my_text_file.txt") do
  {:ok, content} -> IO.puts(content)
  {:error, reason} -> IO.puts("Failed to read file: #{reason}")
end
```

This reads the file and prints its content, but if the file doesn't exist, it prints an error message.

## Deep Dive
Historically, reading files - text or binary - has been vital since the start of programming. Different languages have different techniques with varying complexities.

In Elixir's case, file reading is simplified using the `File.read/1` function, which opens the file, reads it, and then automatically closes it. If you want more control, use `File.open/2` and `IO.binread/2` instead.

Elixir also has nice alternatives for streaming files, great when dealing with large files where you can't or wouldn't want to load everything into memory. 

```Elixir
File.stream!("my_large_file.txt")
|> Enum.each(&IO.puts/1)
```

This reads and prints `my_large_file.txt` line by line, efficiently handling memory.

## See Also
Dive deeper into Elixir's powerful file and IO handling:

- The File module official documentation: https://hexdocs.pm/elixir/File.html
- All about IO and files in Elixir: https://elixir-lang.org/getting-started/io-and-the-file-system.html	
- Real World Elixir: Handling file uploads and downloads: https://www.thegreatcodeadventure.com/elixir-file-upload-download/