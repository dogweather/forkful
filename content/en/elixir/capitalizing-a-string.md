---
title:                "Capitalizing a string"
html_title:           "Elixir recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first character of the string a capital letter. Programmers use this for better readability and data preprocessing, and when the output string must start with a capital letter.

## How to:

In Elixir, we use the `String.capitalize/2` function to capitalize a string. Here's a straightforward example:

```elixir
IO.puts(String.capitalize("elixir"))
```

The output will be:

```elixir
Elixir
```

Piece of cake, right? It not only capitalizes the first character but also turns the rest of the string to lowercase. Handy!

## Deep Dive

Now, let's go a bit deeper. Historically, this functionality isn't unique to Elixir; it exists in most of the other programming languages like Python, JavaScript, etc.

An alternative would be to do the capitalizing manually with `String.slice` but that's reinventing the wheel. Always use built-in functions whenever possible - they're optimized for performance.

As for implementation details, Elixir's `String.capitalize/2` function follows Unicode Standard Annex rules (#29) for sentence boundaries, treating CR, LF, and other paragraph separators as sentence boundaries. Word boundaries aren't treated as sentence boundaries though.

## See Also

Ready to learn more about strings in Elixir? Explore these links:

- Elixir's guide to [Strings and Binaries](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html).
- Documentation for [String.capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)
- Unicode Standard Annex [#29](https://www.unicode.org/reports/tr29/) with sentence boundaries rules.
- More about [Elixir on Wikipedia](https://en.wikipedia.org/wiki/Elixir_(programming_language)).