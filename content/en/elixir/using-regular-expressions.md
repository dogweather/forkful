---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Unraveling Regular Expressions in Elixir

## What & Why?

Regular expressions (regex) are sequences of characters that form a pattern to help in string matching or "searching". Programmers use regex to extract particular information from text, validate input, and transform parts of strings in a flexible and compact way.

## How to:

In Elixir, we use the `Regex` module to work with regular expressions. 

Here's how you match a pattern using `Regex.match?` function:

```elixir
iex> Regex.match?(~r{elixir}, "Hello, Elixir!")
true
```

To replace a part of a string, use the `Regex.replace` function:

```elixir
iex> Regex.replace(~r{elixir}, "Hello, Elixir!", "World")
"Hello, World!"
```

## Deep Dive

Regular expressions date back to the 1950s with early implementations in Unix tools. In Elixir, they follow the Perl Compatible Regular Expressions (PCRE) flavor, appreciated for its rich features and predictability.

The `Regex` module provides Elixir-specific implementations, leveraging Erlang's :re module underneath. It uses the battle-tested PCRE library and supports Unicode.

You could resort to other methods such as string functions (`String.contains?`, `String.replace`, etc.), but regex provides more control and flexibility over complex pattern matching tasks.

## See Also

You can further explore regular expressions in Elixir with these resources:
- Elixir's official guide on `Regex`: https://elixir-lang.org/getting-started/regex.html
- Erlang's documentation for the `:re` module: http://erlang.org/doc/man/re.html
- 'Mastering Regular Expressions' by Jeffrey E.F. Friedl provides a great overview of regex, although not Elixir-specific.