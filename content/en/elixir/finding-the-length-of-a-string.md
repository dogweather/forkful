---
title:    "Elixir recipe: Finding the length of a string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, there are often tasks that require finding the length of a string. Whether it's for data manipulation or input validation, knowing the length of a string can be crucial. In this blog post, we will explore how to find the length of a string in the Elixir programming language.

## How To

To find the length of a string in Elixir, we can use the `String.length()` function. Let's take a look at a simple example:

```Elixir
string = "Hello, world!"

IO.puts String.length(string)

```
The output of this code will be `13`, showing that the string has 13 characters. We can also use string interpolation to dynamically find the length of a string:

```Elixir
name = "John"

greeting = "Hello, #{name}!"

IO.puts String.length(greeting)
```

This will output `9`, as the string "Hello, John!" has 9 characters. It's important to note that this function also considers spaces and special characters when finding the length of a string.

## Deep Dive

Behind the scenes, the `String.length()` function in Elixir utilizes the `String.codepoints()` function to split the string into its individual characters and then counts them. This means that the function supports Unicode characters and can accurately find the length of strings with different languages and characters.

It's also worth mentioning that `String.length()` actually returns the number of codepoints in the string, not the number of characters. Codepoints are the numeric representation of each character in a string.

## See Also

- [`String.length/1` documentation](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir's Unicode and Codepoints guide](https://hexdocs.pm/elixir/master/unicode.html#codepoint-interoperability-in-elixir)
- [Elixir School's Strings lesson](https://elixirschool.com/en/lessons/basics/strings/)

By now, you should have a good understanding of how to find the length of a string in Elixir. Keep in mind that this function can vary in performance depending on the length of the string, as it needs to iterate through each character. Happy coding!