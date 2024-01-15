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

## Why

Why would someone want to concatenate strings in Elixir? Well, concatenating strings allows for the combination of multiple strings into a single string, making it easier to manipulate and display data in a desired format. This can be particularly useful when working with string operations such as building URLs or creating output for user interfaces.

## How To

To concatenate strings in Elixir, we use the `<>` operator, also known as the "string concatenation operator." Let's take a look at some code examples to see how it works.

```Elixir
# Basic concatenation
"Hello " <> "world" 
# Output: "Hello world"

# Concatenation with variables
name = "John"
"Hello, my name is " <> name 
# Output: "Hello, my name is John"

# Concatenation with string interpolation
age = 30
"I am #{age} years old" 
# Output: "I am 30 years old"
```

As you can see, the `<>` operator allows us to combine multiple strings, variables, and even string interpolation to create a new string. It is important to note that the `<>` operator only works with strings and will produce an error if used with other data types.

## Deep Dive

Behind the scenes, concatenation in Elixir is handled by the underlying Erlang VM, which follows certain rules when dealing with strings. This can sometimes lead to unexpected results, particularly when it comes to performance.

One important rule to keep in mind is that strings in Elixir are UTF-8 encoded, meaning each character is represented by one or more bytes. This can lead to longer processing times when concatenating large strings, as the VM needs to allocate memory for the new, larger string and then copy the previous strings into it. In some cases, using a `~s` sigil (to create a string literal) or a `<<>>` binary constructor (to create a binary) can be more performant than using the `<>` operator.

Additionally, if you are concatenating more than two strings, it is recommended to use `List.foldl` for better performance, as it avoids creating intermediary strings. However, for smaller strings, using the `<>` operator is usually more than sufficient.

## See Also

- [Elixir strings documentation](https://hexdocs.pm/elixir/String.html)
- [Erlang string handling](https://erlang.org/doc/programming_examples/strings.html)