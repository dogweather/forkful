---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? 

Converting a string to lower case in Elixir is all about transforming every character in a string to its lower case equivalent. Programmers do this to standardize data, making it easier to compare, search, sort, or analyze.

## How To:

Use `String.downcase/1`, an inbuilt function in Elixir for this task. Here's how to do it:

```elixir
IO.puts String.downcase("Hello, World!")  # "hello, world!"
```

You can see that every character in "Hello, World!", including the 'H' and 'W' have been converted to lower case.

## Deep Dive

The functionality of converting strings to lower case is quite common across different programming languages, which historically stems from a need to facilitate case-insensitive comparisons and searches. 

The `String.downcase/1` function in Elixir uses Unicode character properties to determine the lower case equivalent of a character. If the character doesn't have a lower case equivalent (like symbols or numbers), it remains the same.

If you need a custom string lower-casing operation (for example, for locales that have unique lower-casing rules), there's no inbuilt support in Elixir for that. You would need to use Erlang's `:unicode` module instead.

## See Also

- Elixir's [String module documentation](https://hexdocs.pm/elixir/String.html)
- Function `String.downcase/1` [documentaion](https://hexdocs.pm/elixir/String.html#downcase/1) 
- Erlang's `:unicode` module [documentation](http://erlang.org/doc/man/unicode.html)
- Unicode [properties references](https://unicode.org/reports/tr44/#Property_Index)