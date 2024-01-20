---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is the act of extracting content from a file stored in a text format. Programmers do this for a multitude of reasons like analyzing data, checking logs, or using configuration values that need to be read at runtime.

## How To:

Reading files in Gleam is done using the built-in `file` module's 'read' function. Here's how it's done:

```Gleam
import gleam/ok
import gleam/file.{Error}


pub fn read_my_file() {
 ok.with(file.read("path_to/my_file.txt")) 
 |> result.or_else(|e| case e {
   Error.NotFound -> Ok("> File not found")
   _ -> Ok("> Unexpected error") 
 })
}
```

Let's test it:

```Gleam
read_my_file() == Ok("> File content here...")
```

## Deep Dive

Historically, file I/O was not a trivial task and many programming languages make it complex. Gleam simplifies this process, making it straightforward and easy to handle errors.

Alternatives to reading a text file in Gleam include using other languages like Erlang or Elixir which run on the BEAM, the same runtime system as Gleam, but it's noteworthy that Gleam offers stronger type safety.

Implementation-wise, Gleam's `file.read` function uses the underlying file reading functionality provided by Erlang. This encapsulates the complexity and provides a user-friendly, type-safe method of reading files.

## See Also

2. [Gleam - Getting started guide](https://gleam.run/getting-started/)
3. [Elixir - File docs](https://hexdocs.pm/elixir/File.html)
4. [Erlang - File docs](http://erlang.org/doc/man/file.html)