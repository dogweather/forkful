---
title:                "Interpolating a string"
html_title:           "Elixir recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string is the process of injecting variables into a string. Programmers use it to create dynamic messages and enhance the readability of their code.

## How to:

Let's demonstrate string interpolation in Elixir with code samples. We'll begin with a name variable, then inject it into a greeting. 

```Elixir
name = "Bob"
IO.puts "Hello, #{name}!"
```

Upon executing, you'll see:

```Elixir
Hello, Bob!
```

You could even embed Elixir expressions into the string. Let's multiply 2 by 3 using string interpolation:

```Elixir
IO.puts "Twice of 3 is #{2 * 3}"
```

The output would display:

```Elixir
Twice of 3 is 6
```

## Deep Dive

String interpolation, introduced with Elixir, borrows its concept from other languages like Ruby. It's a more readable alternative to concatenating strings using the `<>` operator. You would rewrite `"Hello, " <> name <> "!"` as `"Hello, #{name}!"`.

Behind the scenes, the `#{}` syntax tells Elixir to evaluate the expression inside and convert it to a string. Note that you can only interpolate into double-quoted strings because single-quoted strings, called charlists, are a different datatype.

## See Also:

- [Elixir School](https://elixirschool.com/en/lessons/basics/basics/) offers a splendid elementary course to grasp Elixir's basics. 
- [Elixir's official guide](https://elixir-lang.org/getting-started/basic-types.html) sheds light on basic types and operations, including string interpolation.
- For further reading, you can delve into how Elixir implements this feature at its core, on the [Elixir GitHub Repository](https://github.com/elixir-lang/elixir).