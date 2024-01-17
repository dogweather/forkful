---
title:                "Extracting substrings"
html_title:           "Elixir recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings in programming refers to the process of obtaining a portion of a string or text. Programmers often do this to manipulate data, perform pattern matching, or simplify string operations.

## How to:

Extracting substrings in Elixir is simple and efficient. To get a portion of a string, use the `String.slice/3` function. 

```
Elixir
# Extracting a single character
String.slice("Hello", 1) # returns "e"

# Extracting a range of characters
String.slice("Hello", 0..2) # returns "Hel"

# Extracting from the end of the string using negative indices
String.slice("Hello", -2..-1) # returns "lo"

# Extracting using a pattern
String.slice("Hello", ~r/[aeiou]/) # returns "e"

```

## Deep Dive

Historically, extracting substrings was used to manipulate text data and perform basic operations such as searching, replacing, and parsing. However, with the rise of regex and other string manipulation libraries, it has become a more powerful tool for pattern matching and data extraction.

Alternative methods for extracting substrings in Elixir include using the `String.split/2` function, which divides a string into a list of substrings, and `String.replace/4` to replace a portion of a string with a new substring.

In terms of implementation, extracting substrings is a straightforward process that utilizes string indexing and slicing. Elixir's native string data type, binary, allows for efficient substring extraction since it is a contiguous sequence of characters.

## See Also

To learn more about extracting substrings in Elixir, check out the [official documentation](https://hexdocs.pm/elixir/String.html#slice/3) or this [Elixir School](https://elixirschool.com/en/lessons/basics/strings/) lesson on strings. You can also explore other string manipulation functions in the [String module](https://hexdocs.pm/elixir/String.html) and the [Regex library](https://hexdocs.pm/elixir/Regex.html).