---
title:                "Elixir recipe: Capitalizing a string"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Writing Elixir programs can be a daunting task, especially for beginners. However, using the right techniques and functions can make your coding experience much smoother and efficient. One such function is the `String.capitalize/1` function, which allows you to easily capitalize strings in your Elixir code. In this blog post, we will delve into why and how to use this function in your Elixir programs.

## How To
To use the `String.capitalize/1` function, simply pass in the string you want to capitalize as an argument. Let's take a look at an example:

```Elixir
string = "elixir"
String.capitalize(string)
```

The output of this code would be "Elixir" as the first letter of the string has now been capitalized. Another useful feature of this function is that it can handle multiple words in a string, capitalizing the first letter of each word. Let's see this in action:

```Elixir
string = "elixir programming"
String.capitalize(string)
```

The output of this code would be "Elixir Programming" as both words have been capitalized.

## Deep Dive
While the `String.capitalize/1` function may seem simple enough, it actually utilizes a more complex underlying logic. In Elixir, strings are represented as lists of characters, and the `String.capitalize/1` function uses this fact to its advantage. It converts the first character of the string to uppercase by using the `1` argument, which signifies the first element in the list. It then appends the remaining elements of the list to the capitalized character, creating the final capitalized string.

Furthermore, the `String.capitalize/1` function also takes into account the Unicode rules for capitalization, making it a reliable choice for handling various languages and special characters.

## See Also
If you're interested in learning more about string manipulation in Elixir, be sure to check out these resources:

- [Elixir's official documentation on strings](https://hexdocs.pm/elixir/String.html)
- [Elixir School's article on manipulating strings](https://elixirschool.com/en/lessons/basics/basics/#string-interpolation-and-manipulation)
- [The Elixir Forum's discussion on string capitalization](https://elixirforum.com/t/capitalizing-the-input-of-a-function/387)

Using the `String.capitalize/1` function may seem like a small detail, but it can greatly improve the readability and organization of your Elixir programs. Give it a try and see how it can enhance your coding experience!