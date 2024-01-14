---
title:    "Elixir recipe: Capitalizing a string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

We all have encountered situations where we need to capitalize a string in our code. Whether it's for formatting purposes or to adhere to a specific naming convention, capitalizing a string is a common task in programming. In this article, we will learn how to easily capitalize a string using Elixir.

## How To

To capitalize a string in Elixir, we can use the `String.capitalize/1` function. This function takes in a string and returns a new string with the first letter capitalized. Let's see some examples:

```Elixir
iex> String.capitalize("hello")
"Hello"

iex> String.capitalize("elixir")
"Elixir"
```

We can also capitalize each word in a string using the `String.capitalize/2` function. This function takes in a string and an optional boolean value which determines whether the rest of the string should be lowercased or not. Let's see an example:

```Elixir
iex> String.capitalize("hello world", true)
"Hello world"
```

If we want to capitalize just the first letter of a string and not the entire word, we can use the `String.upcase/1` function which converts all letters in a string to uppercase:

```Elixir
iex> String.upcase("hello")
"HELLO"

iex> String.upcase("hello world")
"HELLO WORLD"
```

## Deep Dive

Behind the scenes, `String.capitalize/1` and `String.capitalize/2` make use of the `String.capitalize/3` function, which takes in a string, an index, and an optional boolean value. This function replaces the character at the given index with its uppercase version and then converts the rest of the string to lowercase. This is why passing `true` as the second argument in `String.capitalize/2` will capitalize the rest of the string.

In addition, Elixir provides us with the `String.downcase/1` function which converts all letters in a string to lowercase. This function is useful when we want to normalize user input or compare strings in a case-insensitive manner.

## See Also

Here are some useful resources to learn more about Elixir's string functions:

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir String.capitalize/3 Documentation](https://hexdocs.pm/elixir/String.html#capitalize/3)
- [Elixir String Module Cheatsheet](https://devhints.io/elixir-string)

Now that you know how easy it is to capitalize a string in Elixir, go ahead and incorporate it in your code to make it even more robust and user-friendly. Happy coding!