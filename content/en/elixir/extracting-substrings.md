---
title:                "Elixir recipe: Extracting substrings"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are a powerful tool in any programming language, including Elixir. By extracting substrings, you can manipulate and analyze smaller portions of a larger string, making it easier to work with and extract specific information. This can be especially useful when working with text data or user input.

## How To

Extracting substrings in Elixir is a fairly straightforward process. In order to do so, we use the `String.slice/3` function. Let's take a look at an example:

```Elixir
string = "Hello, World!"
substring = String.slice(string, 0..4)
```

In this example, we are extracting the first five characters of the string "Hello, World!". The `String.slice/3` function takes in three arguments: the string to be manipulated, the starting index of the substring, and the ending index of the substring. In this case, we start at index 0 and end at index 4, giving us the substring "Hello".

We can also use negative numbers to specify the index, in which case it will count backwards from the end of the string. Let's see an example of extracting the last three characters of a string:

```Elixir
string = "Hello, World!"
substring = String.slice(string, -3..-1)
```

This time, the substring will be "ld!".

## Deep Dive

The `String.slice/3` function also has an optional fourth argument, the step size, which allows us to extract every nth character from a string. Let's see an example:

```Elixir
string = "123456789"
substring = String.slice(string, 0..8, 2)
```

In this case, we are extracting every second character from the string "123456789", resulting in the substring "13579".

It is also worth mentioning that `String.slice/3` is not limited to just extracting characters. It can also be used to extract words or phrases by using the index of the first and last character of the word or phrase.

## See Also

For more information on strings and manipulating text data in Elixir, check out the following resources:

- [Elixir Strings Documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir String Module Cheat Sheet](https://devhints.io/elixir-string)
- [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/strings/)

Happy coding!