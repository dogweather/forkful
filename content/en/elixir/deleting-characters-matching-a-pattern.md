---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is removing specific characters or sequences from a string based upon a specified pattern (like 'abc', '.', etc). This aids in data cleaning, formatting and simplifying strings, and is a vital tool in a programmer's toolkit.

## How to:

In Elixir, Regex module is used for deleting characters matching a pattern. Here is an example that deletes all vowels from a string.

```elixir
iex> Regex.replace(~r/[aeiou]/, "Hello World", "")
"Hll Wrld"
```

In this case, the pattern is `[aeiou]` which represents all vowels. The `Regex.replace/3` function replaces all occurrences of this pattern in the string "Hello World" with an empty string "", effectively deleting them.

## Deep Dive

This technique has been embedded in programming languages for years. Not limited to Elixir, it is also found, with varying syntax, in Python, Java and many more, highlighting its usefulness and versatility. 

Some alternatives in Elixir include using the `String.replace/3` function, which also replaces occurrences of a pattern, but without the use of regular expressions. For example:

```elixir
iex> String.replace("Hello World", "o", "")
"Hell Wrld"
```

This replaces the 'o' character, but is less flexible, and suited for simpler replacements. 

As for the implementation, the `Regex.replace/3` function in Elixir uses the regular expression engine provided by Erlang, the powerhouse behind Elixir. This built-in functionality makes pattern deletion efficient and reliable.

## See Also

For more on Regex in Elixir, see the [official Elixir documentation](https://hexdocs.pm/elixir/Regex.html).

To learn more about the power of Erlang's regular expression capabilities that Elixir exploits, see the [official Erlang documentation](http://erlang.org/doc/man/re.html). 

For a deeper understanding of regular expressions across programming languages, check out this helpful site: [Regular-Expressions.info](https://www.regular-expressions.info/).