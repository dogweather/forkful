---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first letter of a given string uppercase — if it's a letter. Programmers do it for formatting consistency, user interface polish, or meeting data standards.

## How to:

```elixir
# Capitalize a string in Elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string

# Output will be:
# Elixir programming
```

```elixir
# Capitalize all words in a string
string = "elixir programming language"
capitalized_words = String.split(string)
                    |> Enum.map(&String.capitalize/1)
                    |> Enum.join(" ")

IO.puts capitalized_words

# Output will be:
# Elixir Programming Language
```

## Deep Dive

Back in the early days of computing, programming languages often didn't worry about string manipulation as part of the core language. Elixir, however, comes with a robust module of string functions out of the box, thanks to its roots in the mature Erlang VM (BEAM). Capitalizing strings in Elixir is a breeze with the `String` module.

Beyond the straightforward `String.capitalize/1`, you might encounter scenarios requiring more complex behavior. Say you need to capitalize titles or names in a culturally sensitive manner. Elixir's `String` module alone won't cut it; you'd look toward libraries like `Cldr` for internationalization support.

Under the hood, `String.capitalize/1` takes into account Unicode and multibyte characters, not just ASCII. This means it handles a wide range of languages and alphabets correctly, rather than just English text.

As an alternative, you could roll your own capitalization function, but in most cases, the built-in methods should suffice. With custom implementations, you open the door to subtle bugs, especially with international text. Why reinvent the wheel when you've got high-quality tools ready to go?

## See Also

- Elixir's official `String` documentation: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixir School for learning more about strings and other basics: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
- ExCldr library for internationalization support: [https://hex.pm/packages/ex_cldr](https://hex.pm/packages/ex_cldr)
