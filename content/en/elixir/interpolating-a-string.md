---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

# String Interpolation in Elixir: A Quick Guide

## What & Why?

String interpolation is a process where placeholders in a string are replaced by the values of variables. Programmers use it to weave variable values into strings in a clean, easy-to-read manner.

## How To:

In Elixir, string interpolation is accomplished using `#{}` inside a string (which Elixir calls a `binary`). Here's a brief example:

```Elixir
name = "Jane"
IO.puts "Hello, #{name}!"  
```
The output will be:

```
Hello, Jane!
```

As you can see, the `#{}` acts as a placeholder for the value of the `name` variable.

## Deep Dive

String interpolation in Elixir (and Erlang, the language Elixir is built upon) has its roots in so-called "formatted output functions" of languages like C.

But unlike C, Elixir's string interpolation is more flexible and expressive. You can use it with any data type—not just strings—and you can put arbitrary Elixir code inside the `#{}` sequence.

Instead of string interpolation, you could use concatenation (`<>`) to stitch together strings and variable values. However, for long strings with many variables, interpolation is often more readable.

Under the hood, Elixir's string interpolation works by converting the entire string to be interpolated into a list of smaller strings and variable values. It then joins everything together into the final interpolated string.

## See Also

You can go through these resources for a much deeper understanding:

1. [Elixir Getting Started](https://elixir-lang.org/getting-started/io-and-the-file-system.html#iodots)
2. [Elixir School](https://elixirschool.com/en/lessons/basics/strings/)
3. [Stack Overflow: What is String Interpolation in Elixir?](https://stackoverflow.com/questions/48592674/what-is-string-interpolation-in-elixir)