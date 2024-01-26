---
title:                "Concatenating strings"
date:                  2024-01-20T17:34:26.256384-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means gluing them together end-to-end. Programmers do this to craft messages, create logs, or mash up data for file paths, URLs, and more.

## How to:

In Elixir, you concatenate strings using the `<>` operator. Sounds simple? It is. Check these out:

```elixir
"Hello, " <> "World!" 
# => "Hello, World!"

name = "José"
greeting = "Hola, " <> name <> "!"
# => "Hola, José!"

path = "/home/" <> "user" <> "/documents"
# => "/home/user/documents"
```

Elixir just stitches the strings together in the order you feed them.

## Deep Dive

Back in the day, languages like C used functions like `strcat` for string concatenation—quite a hassle. In Elixir, a modern language built on the Erlang VM, concatenation is simply part of the string game.

Why `<>` and not the plus sign `+`? In Elixir, `+` is strictly for arithmetic. Using `<>` avoids type confusion and errors common in languages where `+` does both jobs.

Under the hood, Elixir strings are binaries. So, when you're mashing strings together, you're really working with binary concatenation, which is efficient and hassle-free.

But there's more to it. Elixir also offers `String` module functions like `String.concat/2` and `IO` functions for building strings which can be handy in more complex scenarios.

## See Also

- [Elixir's String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Programming Elixir by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Elixir School's lesson on Strings](https://elixirschool.com/en/lessons/basics/strings/)
