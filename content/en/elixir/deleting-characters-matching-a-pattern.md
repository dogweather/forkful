---
title:                "Elixir recipe: Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a certain pattern is a common task in many programming languages. In Elixir specifically, this functionality can be incredibly useful when working with strings or lists, as it allows for efficient manipulation of data.

## How To

To delete characters matching a pattern in Elixir, we can use the `String.replace` function. This function takes in the original string, the pattern to match, and the replacement string as arguments.

Let's say we have the following string:

```
input = "Hello, World!"
```

And we want to delete all the commas and exclamation marks from the string. Using `String.replace`, we can achieve this in just one line of code:

```
output = String.replace(input, ~r/[!,]/, "")
```

The result of this would be the string "Hello World" without any commas or exclamation marks. 

We can also use `String.replace` to replace a pattern with a different string. For example, if we want to replace all instances of the word "cat" with "dog" in a string, we can do so with the following code:

```
output = String.replace("I love my cat and my cat loves me", "cat", "dog")
```

This would result in the string "I love my dog and my dog loves me".

## Deep Dive

Under the hood, Elixir's `String.replace` function utilizes the built-in regular expression module to match patterns in a given string. The `~r` syntax denotes a regular expression in Elixir, and the `[!,]` pattern matches any commas or exclamation marks in the string.

It's worth noting that `String.replace` is case-sensitive, so if we want to replace a pattern regardless of case, we can use the `String.replace/4` function and pass in the `:global` option. 

```
output = String.replace("I love my Cat and my cat loves me", ~r/cat/, "dog", global: true)
```

This would result in the string "I love my dog and my dog loves me", with both instances of "cat" being replaced regardless of case.

## See Also

- [Elixir String Documentation](https://hexdocs.pm/elixir/String.html)
- [Regular Expressions in Elixir](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- [String.replace Function](https://hexdocs.pm/elixir/String.html#replace/3)