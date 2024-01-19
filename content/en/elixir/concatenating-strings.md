---
title:                "Concatenating strings"
html_title:           "Elixir recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of joining two or more strings end-to-end to form a new string. We do it to dynamically create sentences, content, or data.

## How to:

In Elixir, we concatenate strings with the `<>` operator. Check this out:

```elixir
name = "Charlie"
greeting = "Hello, " <> name <> "!"
IO.puts greeting
```

The output you'll see is:

```elixir
"Hello, Charlie!"
```

Easy, right? Strings are just glued together in the order you stake them.

## Deep Dive

In the past, Elixir employed concatenation method similar to other languages like JavaScript (`+` operator) or Python (`join` function). However, it now uses the `<>` operator, unique, and more explicit.

Are there alternatives? Well, you could use `String.concat/2` or `String.concat/1` from the built-in String module. But the `<>` operator is faster owing to being a compiler directive.

Why is this important? Concatenation might seem trivial, but it’s frequently used in creating dynamic content, merging pieces of text from different sources, etc. Since Elixir is an immutable language, every concatenation operation creates a new string. Knowing these details helps you manage memory better and write more efficient code.

## See Also

- Official Elixir docs on the String module: [elixir-lang.org/getting-started/basic-types.html#strings](https://elixir-lang.org/getting-started/basic-types.html#strings)
- Discussion on Elixir Forum: [string concatenation vs <> operator](https://elixirforum.com/t/string-concatenation-vs-operator/884)