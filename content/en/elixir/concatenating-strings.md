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

Concatenating strings is the process of combining two or more strings into one larger string. This is a commonly used technique in programming to manipulate and create dynamic text output.

## How to:

Concatenating strings in Elixir is simple and efficient. We can use the `<>` operator to join two strings together. Let's take a look at an example:

```Elixir
name = "John"
greeting = "Hello "

puts greeting <> name
```

This will output: `Hello John`

We can also use the `<>` operator to concatenate multiple strings at once:

```Elixir
first_name = "Jane"
last_name = "Doe"
full_name = first_name <> " " <> last_name

puts full_name
```

This will output: `Jane Doe`

## Deep Dive:

Concatenating strings has been around since the early days of programming. It allows programmers to manipulate text in a more efficient way, compared to manually manipulating each individual character.

In Elixir, there are other ways to concatenate strings as well. One alternative is using the `Enum.join` function, which can be used to join a list of strings together with a delimiter.

The `<>` operator is not limited to only strings, it can also be used to concatenate other data types, such as numbers. This makes it a versatile tool in programming.

## See Also:

- [Elixir Documentation on String Concatenation](https://hexdocs.pm/elixir/String.html#concatenation)
- [Elixir Repl Example of String Concatenation](https://hexdocs.pm/elixir/String.html#concatenation)