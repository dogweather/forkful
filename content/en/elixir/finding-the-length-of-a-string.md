---
title:                "Finding the length of a string"
html_title:           "Elixir recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a language that prioritizes concurrency and scalability, Elixir offers a powerful and efficient way to determine the length of a string. Whether you are developing a web application, parsing data, or manipulating text, knowing the length of a string is a common task that can greatly improve the performance of your code.

## How To

```Elixir
string = "Hello, world!"

# Using the String.length function
String.length(string)

# Output: 13

# Using the String.codepoints function
string
|> String.codepoints()
|> Enum.count()

# Output: 13
```

The easiest way to find the length of a string in Elixir is by using the built-in `String.length` function. This function takes in a string as its argument and returns the length in terms of the number of characters. 

Another approach is to use the `String.codepoints` function, which converts a string into a list of Unicode codepoints. We can then use the `Enum.count` function to determine the number of characters in the list, which is equivalent to the length of the string.

## Deep Dive

Elixir strings are stored using UTF-8 encoding, which means that each character can be represented by one or more bytes. This can lead to inconsistencies when trying to determine the length of a string. For example, depending on the encoding, the word "café" can be represented as either 4 or 5 characters.

To overcome this issue, Elixir provides the `String.graphemes` function, which returns a list of graphemes instead of codepoints. Graphemes refer to the smallest unit of a written language and are more accurate in representing the actual characters in a string.

```Elixir
string = "café"

# Using String.graphemes function
string
|> String.graphemes()
|> Enum.count()

# Output: 4
```

It is important to note that the length of a string can also be influenced by the current locale setting, which defines the language and region preferences of the user. In such cases, it is recommended to use the `String.length/2` function that takes in a second argument for the locale and provides a more accurate length based on the given locale.

## See Also

- Official Elixir documentation on String module: https://hexdocs.pm/elixir/String.html
- Elixir School article on Strings: https://elixirschool.com/en/lessons/basics/string/
- Discussion on determining the length of a string in Elixir: https://stackoverflow.com/questions/39190553/determining-the-length-of-a-string-in-elixir