---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Joining the Dots: An Elixir Approach to String Concatenation 

## What & Why?

String concatenation is the act of connecting two or more strings. It's a key tool in a coder's kit, used to build complex strings from smaller parts, for output formatting, logging, building queries, and more.

## How To:

In Elixir, you use the `<>` operator for string concatenation. Seems super easy huh, let's dive into some examples:

```elixir
str1 = "Hello"
str2 = " World"
IO.puts str1 <> str2
```

When you run this, it'll output: 

```elixir
"Hello World"
```

Note: Unlike some other languages, Elixir won't implicitly convert non-string values. So if you try to concatenate a string with a non-string, you'll get a Type Error.

## Deep Dive 

Back in the day, Erlang didn't treat strings as a basic type - they were just lists of integers representing characters' ASCII codes! Elixir, built on the Erlang VM, introduced the string type with nifty built-in functions like the `<>` operator. 

Instead of `<>`, you could use `String.concat/2` or `String.append/2` for concatenation. 

```elixir
IO.puts String.concat("Hello", " World")
```

This will also output: 

```elixir
"Hello World"
```

Under the hood, when you use `<>` or String functions, Elixir uses the Erlang function `iolist_to_binary/1` to concatenate strings. This function treats strings as potentially large IO data chunks, so it's very efficient, especially for large strings or numerous concatenations.

## See Also

For more details on strings and their manipulation in Elixir, check out these gems:

- [Elixir official docs on Strings](https://hexdocs.pm/elixir/String.html)
- [Elixir School's guide to Basic Strings](https://elixirschool.com/en/lessons/basics/strings)