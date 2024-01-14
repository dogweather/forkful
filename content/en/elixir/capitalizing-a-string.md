---
title:    "Elixir recipe: Capitalizing a string"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why 
One reason for capitalizing a string in Elixir could be for formatting purposes or for following certain naming conventions. It can also make the text easier to read and understand for other developers.

## How To
To capitalize a string in Elixir, we can use the `String.capitalize/1` function. Let's take a look at an example:

```Elixir
name = "john"
capitalized_name = String.capitalize(name)
IO.puts capitalized_name
```
Output: "John"

In this example, we declared a variable named `name` with the value of "john". Then, we used the `String.capitalize/1` function to capitalize the first letter of the string. Finally, we printed the capitalized name using the `IO.puts` function.

We can also capitalize all letters of a string using `String.upcase/1` or convert a string to all lowercase using `String.downcase/1`.

```Elixir
name = "Hello World!"
capitalized_name = String.upcase(name)
IO.puts capitalized_name
```
Output: "HELLO WORLD!"

```Elixir
name = "HeLLo WoRld!"
capitalized_name = String.downcase(name)
IO.puts capitalized_name
```
Output: "hello world!"

## Deep Dive
Behind the scenes, the `String.capitalize/1` function works by splitting the string into individual characters and capitalizing the first one. It then joins the rest of the characters as is and returns the capitalized string. This means that any punctuation or special characters will not be affected.

Additionally, Elixir also provides the `String.capitalize/2` function which allows us to specify the number of characters to capitalize at the beginning of the string. This can be useful if we only want to capitalize certain parts of the string.

## See Also
- [Elixir String.capitalize/1 documentation](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir String.upcase/1 documentation](https://hexdocs.pm/elixir/String.html#upcase/1)
- [Elixir String.downcase/1 documentation](https://hexdocs.pm/elixir/String.html#downcase/1)