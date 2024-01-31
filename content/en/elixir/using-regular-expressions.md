---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions (regex) are patterns used to match character combinations in text. Programmers use them for tasks like validating formats, searching and replacing text, and parsing data from complex strings.

## How to:

In Elixir, you use regex with built-in patterns or by creating your own with the `Regex` module. Here's a quick example:

```elixir
# Searching for the word "hello"
regex = ~r/hello/
"hello world" =~ regex
# => true

# Case-insensitive search
regex = ~r/hello/i
"Hello world" =~ regex
# => true

# Replacing "world" with "Elixir"
"hello world" |> String.replace(~r/world/, "Elixir")
# => "hello Elixir"
```

## Deep Dive

Regex was pioneered in the 1950s by mathematician Stephen Kleene. Elixir implements regex through the PCRE (Perl Compatible Regular Expressions) library, which matches patterns robustly. Alternatives such as string matching with `String.contains?/2` or `String.starts_with?/2` exist, but they lack the flexibility regex offers. Elixir's `Regex` module compiles patterns to an internal format optimized for repeated use, saving computation time.

## See Also

- Elixir's `Regex` module documentation: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Regex101, an online regex tester and debugger: [https://regex101.com/](https://regex101.com/)
- "Programming Elixir" by Dave Thomas - a comprehensive guide that also covers regex use.
