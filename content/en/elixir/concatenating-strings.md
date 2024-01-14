---
title:                "Elixir recipe: Concatenating strings"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation, the process of combining two or more strings into one, is a common task in programming. It allows us to create dynamic and informative output by joining different pieces of text together. In this blog post, we will explore how to concatenate strings in Elixir and why it is useful.

## How To

In Elixir, we can use the `<>` operator to concatenate strings. Let's take a look at a simple example:

```Elixir
name = "John" <> " " <> "Doe"
IO.puts(name)
```

The output of this code would be `John Doe`, as the three strings are joined together to create a new string. We can also use the `<<>>` syntax to concatenate strings, like so:

```Elixir
"Greetings, " <> "my" <<>> "friend"
```

This would result in `Greetings, my friend`.
We can also concatenate variables together, as shown in the example below:

```Elixir
first_name = "John"
last_name = "Doe"
full_name = first_name <> " " <> last_name
```

Here, the variable `full_name` would contain the string `John Doe`. It's important to note that the `<>` operator is not limited to just strings. It can be used to concatenate any type of data, including numbers and lists.

## Deep Dive

Behind the scenes, the `<>` operator in Elixir is actually calling the `Kernel.++/2` function. This function takes two arguments, the two strings to be concatenated, and returns a new string with the two joined together.

It's also worth mentioning that in Elixir, strings are stored as UTF-8 encoded binaries. This means that when concatenating strings, Elixir will first convert them to binaries before joining them together.

## See Also

- Official Elixir documentation on string concatenation: https://hexdocs.pm/elixir/String.html#concatenation
- Elixir School's lesson on strings: https://elixirschool.com/en/lessons/basics/strings/

Now that you have a better understanding of how to concatenate strings in Elixir, go forth and create dynamic and informative output in your programs!