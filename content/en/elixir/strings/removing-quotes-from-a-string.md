---
aliases:
- /en/elixir/removing-quotes-from-a-string/
date: 2024-01-25 20:50:15.047489-07:00
description: "Stripping quotes from a string means shedding off those extra wrappers\
  \ to get the clean text inside. Programmers do this to sanitize input, avoid errors,\u2026"
lastmod: 2024-02-18 23:09:10.753694
model: gpt-4-1106-preview
summary: "Stripping quotes from a string means shedding off those extra wrappers to\
  \ get the clean text inside. Programmers do this to sanitize input, avoid errors,\u2026"
title: Removing quotes from a string
---

{{< edit_this_page >}}

## What & Why?
Stripping quotes from a string means shedding off those extra wrappers to get the clean text inside. Programmers do this to sanitize input, avoid errors, and prep data for processing where quotes are nuisances, not features.

## How to:
Elixir has no built-in 'remove quotes' function, but it's a cinch to roll your own with pattern matching or `String` functions. See these snippets:

```elixir
# Using pattern matching
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Sample Usage
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# Using String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Sample Usage
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

Output for both methods will be:
```
"Hello, World!"
```

## Deep Dive
Back in the day, quotes in strings were a minefield—mishandle them, and boom, syntax errors or security holes. In Elixir, pattern matching treats your strings like Lego blocks, letting you pick apart and rebuild with precision. Its robust `String` module comes in handy too, flexibly nixing quotes with `trim` functions. The alternatives? Regular expressions can kick quotes to the curb, and external libraries might pack extra firepower if you need more than basic stripping.

## See Also
Dive deeper with these:
- [Elixir's String module](https://hexdocs.pm/elixir/String.html)
- [Learn more about pattern matching in Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Regular expressions in Elixir (Regex module)](https://hexdocs.pm/elixir/Regex.html)
