---
title:                "Interpolating a string"
aliases:
- en/elixir/interpolating-a-string.md
date:                  2024-01-20T17:50:37.743188-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you insert variables or computations into a string. It's handy for building strings dynamically without the mess of concatenation.

## How to:
```elixir
name = "Josie"
age = 28

# Interpolating variables
greeting = "Hello, #{name}! You are #{age} years old."
IO.puts greeting
```
Sample output:
```
Hello, Josie! You are 28 years old.
```
```elixir
# Interpolating expressions
IO.puts "In five years, #{name} will be #{age + 5} years old."
```
Sample output:
```
In five years, Josie will be 33 years old.
```

## Deep Dive
In the early days, you'd glue strings together with `+` or `,`. It was a pain. Languages then started to use interpolation for a cleaner, more readable approach. Elixir, being a modern language, also supports this feature natively.

Here's what's going on under the hood with `"Hello, #{name}!"`: during compilation, Elixir transforms the string into a concatenation of binary parts, which is efficient because binaries in Elixir are immutable.

Alternative ways to handle strings without interpolation in Elixir might include using the `String.concat/2` or the `<>` operator, but these methods are less ergonomic for complex strings.

The interpolation syntax `"#{...}"` can include any Elixir expression, which is evaluated and then converted to a string. This is possible due to Elixir being dynamically typed and having first-class support for expressions in its strings. But remember, it’s best kept for simpler expressions to maintain readability.

## See Also
- Elixir's `String` module documentation: https://hexdocs.pm/elixir/String.html
- A guide to Elixir's binary data type: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
