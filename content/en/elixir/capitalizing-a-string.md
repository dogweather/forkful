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

Capitalizing a string in programming refers to changing the first character of a string to an uppercase, if it's not already. Programmers do this often for formatting purposes, such as for user interface text or data presentation.

## How to:

Capitalizing a string in Elixir is an untroubled task. It can be done by using the `capitalcase()` function from the String module. This function takes a string and does exactly what we just mentioned - it changes the first character to an uppercase.

```Elixir
IO.puts String.capitalize("elixir")
# Output: > Elixir
```
So, `String.capitalize("elixir")` returns "Elixir".

## Deep Dive

Historically, the capitalize function has roots in the infancy of text editing software. Having texts consistently formatted, which includes capitalizing at appropriate places, was deemed important for readability.

While there's a built-in function in Elixir for this, alternative methods exist too. For instance, you might write your own capitalize function, or possibly use the `upcase()` function in conjunction with slicing the string and then concatenating. But that's needless work in Elixir because `String.capitalize()` takes care of that elegantly, by using Unicode character properties to accurately capitalise all sorts of characters.

Elixir, being a highly concurrent language, does a great job here. Each string in Elixir is stored as a binary, and the capitalization operation is performed on the binary representation of the string. This underpins Elixir's excellent string handling capabilities.

## See Also

Follow these links for additional details and functions related to Elixir Strings:

1. Elixir String module documentation: [String | Elixir](https://hexdocs.pm/elixir/String.html)
2. More about Unicode Character Properties in Elixir: [Unicode Syntax | Elixir](https://elixir-lang.org/getting-started/unicode-and-strings.html#unicode-syntax-in-elixir)
3. Detailed guide on Elixir’s string handling : [Working with strings in Elixir | Erlang Solutions Blog](https://www.erlang-solutions.com/blog/strings-and-character-data-in-elixir.html)