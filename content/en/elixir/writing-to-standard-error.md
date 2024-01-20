---
title:                "Writing to standard error"
html_title:           "Elixir recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) involves sending data not to the usual output (stdout), but to a separate channel intended for error messages. Programmers do this to separate the normal execution flow from error reporting, which aids in troubleshooting.

## How to:

Code to print to the stderr in Elixir is straightforward and executed via Erlang's `:io.stderr`:

```elixir
:io.format(:standard_error,'~s', ['Hello, stderr!\n'])
```

When you run this, "Hello, stderr!" will be printed to your standard error output.

## Deep Dive

In historical context, stderr was part of the three Unix standard data streams built into most programming environments — stdin (standard input), stdout (standard output), and stderr (standard error). 

Alternatives to `:io.format()` are using other libraries like the Logger library in Elixir:

```elixir
require Logger
Logger.error "Error message for stderr"
```
This has the added advantage of being configurable and supported across different logging backends.

In terms of implementation, writing to stderr in Elixir is straightforward because Elixir runs on the BEAM (the Erlang virtual machine). As a result, it has access to a host of powerful features from the underlying Erlang system, including robust error handling mechanisms such as stderr.

## See Also

1. More about `io.format`: <https://hexdocs.pm/elixir/1.12/IO.html#format/3>
2. Logger library in Elixir: <https://hexdocs.pm/elixir/Logger.html>
3. Information on Unix standard streams: <https://en.wikipedia.org/wiki/Standard_streams>
4. more on BEAM and error handling: <https://ferd.ca/the-zen-of-erlang.html>