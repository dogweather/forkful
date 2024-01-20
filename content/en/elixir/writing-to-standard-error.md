---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (`stderr`) is outputting text that’s not part of the main program data but indicates errors or diagnostics. Programmers do it to debug and log issues without cluttering the standard output (`stdout`), which is often reserved for program's result data.

## How to:

To write to `stderr` in Elixir, use `IO.warn/1` or `IO.puts/2`. Here's how:

```elixir
# Write to stderr with IO.warn
IO.warn("Something went wrong!")

# Write to stderr with IO.puts
IO.puts(:stderr, "Detailed error information.")
```

Sample output to `stderr`:

```
Something went wrong!
Detailed error information.
```

## Deep Dive

Historically, separating `stderr` from `stdout` allowed Unix users to handle error messages distinctly from regular output, which could be especially useful when redirecting output to a file or another program.

Elixir, being a modern language, maintains this tradition. While `IO.puts/1` defaults to `stdout`, passing the `:stderr` atom as the first argument switches the stream. `IO.warn/1` writes to `stderr` by default, which suits warning messages.

Alternatives for error logging in Elixir might include the Logger module for a more structured approach. This can be configured to write logs of various levels to `stderr`.

Under the hood, Elixir's IO functions for stderr and stdout interact with Erlang's :io module, which in turn works with the underlying operating system's I/O streams.

## See Also

- [Elixir's IO Module Documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir's Logger Module Documentation](https://hexdocs.pm/logger/Logger.html)
- [Erlang's :io Module Documentation](http://erlang.org/doc/man/io.html)