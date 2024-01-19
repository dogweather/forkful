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

## What & Why?

Writing a text file is the process of creating and filling a .txt file using code. We do it to store data persistently, automate document generation, or output the results of operations.

## How to:

In Elixir, we use `File.write/2`. It needs a file path and content.

```Elixir
{:ok, _} = File.write("your_file.txt", "Some text")
```

Done! Now you've got "your_file.txt". It says, "Some text".

Another way, write lines as a list:

```Elixir
{:ok, _} = File.write("your_file.txt", ["A line", "Another line"])
```

The lines will be simply concatenated, no line breaks added.

If you need to append text instead of totally rewriting it, use `File.write/3`:

```Elixir
{:ok, _} = File.write("your_file.txt", "Some appended text", [:append])
```

Here, `:append` means, "Don't delete what was there."

## Deep Dive

Elixir's `File.write/*` is straightforward due to Erlang's powerful 'file' module, borrowing its simple design.

If Elixir isn't your only language, Python's `open` with `'w'` flag, and Java's `FileWriter` work like `File.write/2`, and their 'append' versions like `File.write/3`.

Elixir writes text files in binary by default. "Binary" here doesn't mean unreadable non-text; it means the OS won't translate line endings. Thus, on Windows, you may want to write lines with `\r\n` endings, and on Unix-style systems, just `\n`. To insert such line endings when writing lines, you'll need to join them with the appropriate line ending:

```Elixir
{:ok, _} = File.write("your_file.txt", ["A line", "Another line"] |> Enum.join("\n"))
```

## See Also

Check out Erlang's 'file' documentation for everything Elixir's `File` can do: http://erlang.org/doc/man/file.html

Also, Elixir's `File` has even more handy functions hidden in its own doc: https://hexdocs.pm/elixir/File.html

A good text file can be just a part of good logging. Read 'Logger' next: https://hexdocs.pm/elixir/Logger.html