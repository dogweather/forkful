---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text refers to the algorithmic process of locating certain strings within a text and replacing it with another. This comes in handy when programmers need to modify large bodies of code or text documents without manually editing each occurrence.

## How to:

Searching and replacing text in Elixir involves using the `Regex.replace/3` function from the `Regex` module. Here's a simple example of how to use it:

```Elixir
string = "Hello there, programmer!"
IO.puts Regex.replace(~r/programmer/, string, "coder")

# Output: "Hello there, coder!"
```

Here, the `Regex.replace/3` function is used to replace the word "programmer" with "coder" in the given string. Note the `~r/.../` syntax is used to define the regular expression.

## Deep Dive

Historically, searching and replacing text has been a cornerstone in text processing and manipulation, highly used in several areas, from database management to browser search functionality.

Several approaches exist for text search and replace, including Regular Expressions, Boyer-Moore, and Knuth-Morris-Pratt algorithms. Each has its advantages. For example, Regular Expressions are more flexible, while others are faster for large bodies of text.

In Elixir, the `Regex` module provides the functionality to search and replace text. Regular Expressions are patterns used for matching subsets of strings, described by a sequence of characters. The `replace/3` function's implementation involves traversing the text and replacing every occurrence of the pattern with the provided replacement. Keep in mind concurrent modifications when working with large mutable texts.


## See Also

For more in-depth understanding:

- Elixir Doc: [Regex Module](https://hexdocs.pm/elixir/Regex.html)
- Overview of algorithms for text search and replace: [Wikipedia](https://en.wikipedia.org/wiki/String_searching_algorithm)