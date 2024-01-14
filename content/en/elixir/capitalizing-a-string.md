---
title:                "Elixir recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

One of the most common tasks in programming is manipulating strings. In Elixir, a popular functional programming language, there are various ways to manipulate strings, including capitalizing them. Capitalizing a string is useful for formatting data, displaying titles, and more. In this blog post, we will explore how to capitalize a string in Elixir, and the different methods available for doing so.

## How To
To capitalize a string in Elixir, we can use the String.capitalize function. Let's take a look at an example:

```Elixir
iex> String.capitalize("elixir")
"Elixir"
```

As we can see, the first letter of the string has been capitalized. We can also use this function to capitalize multiple words, such as in a title:

```Elixir
iex> String.capitalize("elixir programming")
"Elixir programming"
```

The first letter of each word in the string has been capitalized, which is the standard for titles.

Another way to capitalize a string in Elixir is by using the String.to_title function. This function performs the same task as the String.capitalize function, but it also capitalizes the first letter after symbols, such as spaces and dashes. Let's see an example:

```Elixir
iex> String.to_title("my favorite-elixir programming language")
"My Favorite-Elixir Programming Language"
```

As we can see, the first letter of each word and the letter after the dash have been capitalized. This is useful when dealing with strings that have symbols in them.

## Deep Dive
To truly understand how capitalizing a string works in Elixir, it's important to know a bit about how strings are represented in the language. In Elixir, strings are represented as binaries, which are sequences of bytes. This means that when we capitalize a string, we are actually modifying the individual bytes in the string.

It's also worth noting that the String.capitalize and String.to_title functions are both Unicode aware. This means that they will correctly capitalize strings that contain non-ASCII characters, such as accented letters or emoji.

## See Also
- [Elixir String.capitalize documentation](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir String.to_title documentation](https://hexdocs.pm/elixir/String.html#to_title/1)
- [Elixir Strings guide](https://elixir-lang.org/getting-started/string.html)