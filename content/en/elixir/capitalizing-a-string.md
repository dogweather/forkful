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

## What & Why?

In Elixir, capitalizing a string means converting all the letters in a string to uppercase. Programmers often do this to standardize the formatting of string data or to ensure that the inputs for certain functions are always in the same case.

## How to:

To capitalize a string in Elixir, we can use the ```String.upcase/1``` function. Let's see an example:

```Elixir
String.upcase("hello world")
```

This will output:

```Elixir
"HELLO WORLD"
```

If we want to capitalize only the first letter of a string, we can use the ```String.capitalize/1``` function:

```Elixir
String.capitalize("elixir")
```

The output will be:

```Elixir
"Elixir"
```

## Deep Dive

Capitalizing strings is a common practice in programming, not only in Elixir but in many other languages as well. In some cases, it is necessary for data validation and in others, it simply serves as a formatting convention.

In Elixir, there are also other ways to convert strings to uppercase or lowercase. For example, we can use the ```String.to_upper/1``` or ```String.to_lower/1``` functions, which have the same effect as ```String.upcase/1``` and ```String.downcase/1```.

## See Also

To learn more about string manipulation in Elixir, check out the official documentation on strings: https://hexdocs.pm/elixir/1.11/String.html. Additionally, you can explore other functions in the String module that might be useful for your projects.