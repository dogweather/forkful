---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means getting specific portions of a string. We do it to focus on or manipulate distinct characters/groups of characters within a larger text context. 

## How To:

Elixir offers various methods to extract substrings. Let's rapidly see the practical examples.

To get a substring from a given position to the end of the string:

```elixir
iex> String.slice("Hello, World!", 7..-1)
"World!"
```

To get a substring within specific positions (start from 0):

```elixir
iex> String.slice("Hello, World!", 0..4)
"Hello"
```

## Deep Dive:

Elixir is built on top of Erlang, and in early versions of Erlang, strings were simply lists of ASCII values. This was limiting and inefficient in terms of processing, leading to the introduction of binaries - more memory-efficient structures for storing strings.

While extracting substrings in Elixir, there's one critical thing to remember: Elixir uses 0-based indexing, like many other programming languages. That means the first character of the string is at position 0, not 1.

It's also worth noting that, instead of using substring extraction, you could use pattern matching to extract parts of a string based on regular expressions. This is a more sophisticated option that can be more flexible in complex scenarios.

## See Also:

Here're a couple of resources that will aid in your Elixir journey:

- Elixir's [String module documentation](https://hexdocs.pm/elixir/String.html) for complete string handling features
- [Elixir School](https://elixirschool.com/en/), a comprehensive open-source guide to Elixir
- For an alternative, advanced approach to handle strings, delve into [Elixir Regex documentation](https://hexdocs.pm/elixir/Regex.html)