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

Extracting substrings is the process of retrieving a portion of a string based on their index or a pattern. Programmers extract substrings for various tasks including parsing user input, extracting data, and manipulating text.

## How to:

In Elixir, you can extract substrings using the `String.slice/2` function. The function takes a string and a range as input and returns the extracted substring.

use it like this

```elixir
iex> String.slice("Hello, world!", 0..4)
"Hello"
```

To get a substring from the end of the string, use negative indices:

```elixir
iex> String.slice("Hello, world!", -6..-2)
"world"
```

If the range is out of the string's boundaries, it'll adjust the range to fit within the string:

```elixir
iex> String.slice("Hello, world!", 50..100)
""
iex> String.slice("Hello, world!", -100..4)
"Hello"
```

## Deep Dive

Elixir's `String.slice/2` function comes from its Erlang roots, and it's part of its excellent text manipulation capabilities. Like many other Elixir functions, it protects the programmer from common edge cases and errors through its graceful handling of ranges that exceed the string's boundaries.

There are other ways to extract substrings depending on the context and your specific needs. For example, you can use the `String.split/2` function if you're looking to divide the string around a particular character or pattern:

```elixir
iex> String.split("foo/bar/baz", "/")
["foo", "bar", "baz"]
```

Under the hood, `String.slice/2` leverages the power of Elixir's binary patterns to efficiently manipulate bits and bytes, which is one of this language's strengths.

## See Also

To learn more about Elixir's string manipulation functions, check out the following resources:

- Official `String` module documentation: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixir's binary pattern matching: [https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- The `iex` tool (Elixir's interactive shell) for practicing and testing out code: [https://hexdocs.pm/iex/IEx.html](https://hexdocs.pm/iex/IEx.html)