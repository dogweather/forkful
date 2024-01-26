---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:41:59.535072-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is all about finding specific sequences of characters and getting rid of them. Programmers do this to sanitize data, format content, or manipulate strings in a way that aligns with their specific needs.

## How to:

In Elixir, use the `String.replace/4` function to delete characters matching a pattern. Check out these samples:

```elixir
# Delete digits from a string
original_string = "Elixir2023Rocks!"
clean_string = String.replace(original_string, ~r/\d/, "")
IO.puts(clean_string) # Output: "ElixirRocks!"

# Remove punctuation
punctuationless_string = String.replace(original_string, ~r/[[:punct:]]/, "")
IO.puts(punctuationless_string) # Output: "Elixir2023Rocks"

# Strip out whitespace
no_whitespace_string = String.replace(original_string, ~r/\s/, "")
IO.puts(no_whitespace_string) # Output: "Elixir2023Rocks!"
```

## Deep Dive

The use of pattern matching to delete characters in strings is not unique to Elixir; it's a common feature in nearly all programming languages, evolved from regular expression (regex) capabilities in early Unix tools like `sed` and `grep`. Alternatives to `String.replace/4` could be using pattern matching and recursion to manually traverse and modify a string, but this method is generally more verbose and complex, making built-in regex functions a go-to. Under the hood, `String.replace/4` leverages Elixir's Erlang heritage, utilizing the powerful pattern matching and string manipulation abilities of the BEAM virtual machine.

## See Also:

- Elixir `String` module documentation: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex in Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- 'Learn Regular Expressions': [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- Elixir School's take on strings and pattern matching: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
