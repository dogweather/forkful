---
date: 2024-01-25 03:40:02.322850-07:00
description: "An interactive shell, or REPL (Read-Eval-Print Loop), lets you try out\
  \ code snippets in real-time. Elixir programmers use the REPL, called IEx\u2026"
lastmod: '2024-03-13T22:44:59.783973-06:00'
model: gpt-4-1106-preview
summary: An interactive shell, or REPL (Read-Eval-Print Loop), lets you try out code
  snippets in real-time.
title: Using an interactive shell (REPL)
weight: 34
---

## What & Why?
An interactive shell, or REPL (Read-Eval-Print Loop), lets you try out code snippets in real-time. Elixir programmers use the REPL, called IEx (Interactive Elixir), for experimenting, debugging, and learning the language.

## How to:
To launch IEx, open your terminal and type `iex`. Here's a taste:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Output should show variable assignment, function results, and an anonymous function at work.

## Deep Dive
The IEx shell has been a part of Elixir since its early days. José Valim, the creator of Elixir, drew inspiration from the interactive shells of other languages like Python's `python` and Ruby's `irb`. While IEx shares many features with these, it is built to handle Elixir's concurrent nature and is fully integrated with the Erlang VM capabilities.

Alternatives to IEx in the Erlang ecosystem include `erl`, the Erlang shell. But IEx provides a more Elixir-friendly environment, with features like comprehensive tab completion, history, and helpers.

The IEx REPL is more than a playground; it can seamlessly connect to a running system. This is crucial for debugging live applications. The underlying implementation relies on the BEAM (the Erlang VM), ensuring features like hot code swapping are supported right in the shell.

## See Also
Check these out for further reading and resources:

- [Elixir's IEx documentation](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - The Elixir Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlang's `erl` documentation](http://erlang.org/doc/man/erl.html)
- [Learning Elixir’s Interactive Shell](https://elixirschool.com/en/lessons/basics/iex_helpers/)
