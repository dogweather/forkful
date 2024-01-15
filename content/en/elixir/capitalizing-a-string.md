---
title:                "Capitalizing a string"
html_title:           "Elixir recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming, especially when dealing with user input or data processing. In Elixir, there are built-in functions that can easily help with this task, making it a quick and efficient process.

## How To

To capitalize a string in Elixir, we can use the `String.capitalize` function. Let's take a look at some examples:

```
# Simple example
iex> String.capitalize("hello world")
"Hello world"

# Capitalizing only the first letter
iex> String.capitalize("hello world", :first)
"Hello world"

# Capitalizing each word
iex> String.capitalize("hello world", :words)
"Hello World"

# Handling Unicode characters
iex> String.capitalize("élixir")
"Élixir"
```

As you can see, the `String.capitalize` function takes two arguments: the string to be capitalized and an optional keyword argument specifying the type of capitalization to be applied. If no keyword argument is provided, it capitalizes the first letter by default.

## Deep Dive

Behind the scenes, the `String.capitalize` function uses the `:unicode` module to correctly handle Unicode characters. This means that it can properly capitalize not only English letters, but also letters from other languages.

It's worth noting that the `String.capitalize` function will not modify any characters that are not considered letters. For example, if we have a string with numbers or special characters, those will remain unchanged.

See Also

- Official Elixir documentation for `String.capitalize`: https://hexdocs.pm/elixir/String.html#capitalize/2
- Other useful string manipulation functions in Elixir: https://hexdocs.pm/elixir/String.html#content