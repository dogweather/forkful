---
title:                "Reading command line arguments"
date:                  2024-01-20T17:55:46.387430-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments lets a program grab data right from the terminal—stuff like settings or file names. Programmers do it to customize a program's behavior without changing the code.

## How to:

In Elixir, grabbing command line arguments is a no-brainer. Use `System.argv()` and you'll snag them as a list of strings.

```elixir
defmodule CliArgs do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CliArgs.main()
```

Run this being like `elixir cli_args.exs foo bar baz`, and expect:

```
["foo", "bar", "baz"]
```

You're seeing the arguments `foo`, `bar`, and `baz` right there in an Elixir list.

## Deep Dive

Historically, command line arguments are as old as the hills, stemming from early CLI environments. In Elixir, `System.argv()` is your trusty steed for this job. Why? Because it's baked into the Erlang VM, which Elixir is built on top of. 

Alternatives? Sure, you've got libraries that parse arguments for you, adding flags and options. But for vanilla Elixir, `System.argv()` is the way to go.

Implementation wise, it's important to remember that `System.argv()` gives you all arguments as strings. If you need numbers or other types, you'll have to convert them manually. Also, the order matters. Your first command line argument is `List.first(System.argv())`, and so on.

## See Also

For more, check out:
- [Elixir's System module docs](https://hexdocs.pm/elixir/System.html) for other handy system-related functions.
- [Optparse](https://hexdocs.pm/elixir/OptionParser.html) in Elixir’s standard library, which is a beast for parsing command line options.
- [Erlang's init docs](http://erlang.org/doc/man/init.html) if you’re curious about the under-the-hood VM magic that supports Elixir.