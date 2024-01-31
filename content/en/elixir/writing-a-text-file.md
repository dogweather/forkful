---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file involves saving data to a file in a human-readable format. Programmers do this to persist info like logs, settings, and data output.

## How to:

Elixir makes writing text files straightforward. Here's a simple example of writing to a file named "hello.txt".

```elixir
File.write("hello.txt", "Hello, World!\n")
```
After running this, check "hello.txt" and it'll read:

```
Hello, World!
```
For appending text instead of overwriting:

```elixir
File.write("hello.txt", "Another line!\n", [:append])
```

Now "hello.txt" will show:

```
Hello, World!
Another line!
```

## Deep Dive

Elixir's approach to file writing reflects its Erlang heritage, focusing on reliability and concurrency. Alternatives include using streams for larger data. Internally, Elixir uses Erlang's :file module which interacts with the underlying OS.

## See Also

- Elixir `File` module docs: https://hexdocs.pm/elixir/File.html
- Erlang `:file` module docs: https://erlang.org/doc/man/file.html 
- Learn about Elixir's Stream module for handling large data: https://hexdocs.pm/elixir/Stream.html
