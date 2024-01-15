---
title:                "Deleting characters matching a pattern"
html_title:           "Elixir recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern is a common task in programming, especially when working with strings. This can help clean up data or filter out unwanted characters, making it easier to work with. It can also be useful when dealing with user input, ensuring that it meets certain criteria.

## How To

To delete characters matching a pattern in Elixir, we can use the `String.replace/3` function. This function takes in three arguments: the string to be modified, the pattern to match, and the replacement string. Here's an example:

```Elixir
string = "HelloWorld"
String.replace(string, "l", "") #=> "HeoWord"
```

In this example, we replace all instances of "l" in the string with an empty string, effectively deleting them. We can also use regular expressions as the pattern to match. For example, if we want to delete all numbers from a string:

```Elixir
string = "1a2b3c4d5e"
String.replace(string, ~r/[0-9]/, "") #=> "abcde"
```

We can also use the bang `!` version of `String.replace` to delete characters in place, instead of returning a new string.

## Deep Dive

The `String.replace/3` function takes an additional optional argument for the number of replacements to be made. By default, it replaces all occurrences of the pattern. We can specify a different number if we only want to delete a certain number of characters. For example:

```Elixir
string = "1a2b3c4d5e"
String.replace(string, ~r/[0-9]/, "", 2) #=> "ab3c4d5e"
```

This will only replace the first two numbers in the string. We can also use the `:global` option to replace all occurrences, even if they are in different parts of the string. Lastly, we can also use `String.replace/4` to pass in a function as the replacement, allowing for more complex manipulation.

## See Also

- [String.replace documentation](https://hexdocs.pm/elixir/String.html#replace/3)
- [Regular expressions in Elixir](https://hexdocs.pm/elixir/Regex.html)