---
aliases:
- /en/elixir/organizing-code-into-functions/
date: 2024-01-25 03:00:07.632904-07:00
description: "Organizing code into functions means chunking related operations into\
  \ reusable blocks. We do it to improve readability and maintainability, reduce\u2026"
lastmod: 2024-02-18 23:09:10.771044
model: gpt-4-1106-preview
summary: "Organizing code into functions means chunking related operations into reusable\
  \ blocks. We do it to improve readability and maintainability, reduce\u2026"
title: Organizing code into functions
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions means chunking related operations into reusable blocks. We do it to improve readability and maintainability, reduce duplication, and simplify testing.

## How to:
Let's whip up a simple Elixir function to capitalize words:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Output:
```
Hello Elixir World
```
Here, we've neatly packaged the word capitalization logic into a function called `capitalize_words`.

## Deep Dive
In Elixir, and the broader Erlang VM ecosystem, functions are first-class citizens, inheriting the philosophy of breaking down problems into smaller, manageable, and isolated pieces. Historically, this functional approach has roots in lambda calculus and Lisps, promoting code as data philosophy.

Alternatives to organizing code can be using macros or processes in Elixir for repetitive or concurrent tasks, respectively. Implementation-wise, Elixir functions can handle pattern matching and receive different arguments (arity), granting them versatility.

## See Also
- [Elixir's official documentation on functions](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
