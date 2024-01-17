---
title:                "Converting a string to lower case"
html_title:           "Elixir recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case is the process of transforming every uppercase letter in a string to its corresponding lowercase letter. Programmers often do this to standardize input and make it easier to compare strings, as lowercase and uppercase letters are treated as distinct characters in most languages.

## How to:
To convert a string to lower case in Elixir, we can use the `String.downcase/1` function. This function takes in a string as an argument and returns a new string with all letters transformed to lowercase. 

```
Elixir String.downcase("HELLO WORLD") 
```

Output:
```
 "hello world"
```

We can also use the `String.downcase/2` function to specify a specific character set for the conversion. For example, if we want to convert a string to lowercase but preserve any accented characters, we can use the `ascii` option.

```
Elixir String.downcase("Café", :ascii)
```

Output:
```
"Café"
```

## Deep Dive:
In the past, converting strings to lower case was a common task in programming languages as it was necessary for case-sensitive operations. However, with the rise of Unicode, which supports both uppercase and lowercase versions of each character, the need for case conversion has decreased.

Some alternative methods for converting strings to lower case in Elixir include using the `Kernel.to_lower/1` function or explicitly converting each character using the `String.to_charlist/1` function.

The `String.downcase/1` function in Elixir uses the `String.Unicode.downcase/1` function under the hood, which converts strings based on the Unicode standard. This ensures that strings are converted accurately, even for characters outside of the ASCII character set.

## See Also:
- [Official Elixir documentation on String module](https://hexdocs.pm/elixir/String.html)
- [Source code for `String.downcase/2` function](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/string.ex#L1752-L1756)
- [Unicode Standard](https://www.unicode.org/standard/standard.html)