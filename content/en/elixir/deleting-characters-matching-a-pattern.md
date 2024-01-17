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

## What & Why?

Deleting characters that match a specific pattern is a common task in programming. It involves removing specific characters from a string or text that meet a certain condition or match a given pattern. Programmers often need to delete characters matching a pattern to clean up or extract specific information from a given text.

## How to:

To delete characters matching a pattern in Elixir, we can use the `String.replace/3` function. This function takes three arguments: the original string, the pattern to be matched, and the replacement string. The function will then replace all instances of the pattern with the replacement string.

```Elixir 
# Example 1: Removing vowels from a text
text = "Hello, how are you?"
String.replace(text, ~r/[aeiou]/, "")

# Output: "Hll, hw r y?"

# Example 2: Removing numbers from a string
string = "Elixir123"
String.replace(string, ~r/[0-9]/, "")

# Output: "Elixir"
```

## Deep Dive:

In Elixir, regular expressions can be used to create patterns to match specific characters. Regular expressions, often shortened to "regex", are a powerful tool for pattern matching and are widely used in programming to manipulate strings. Elixir uses the `~r` sigil to represent regular expressions and the `~r/<pattern>/` syntax to define a regex pattern. Alternative ways to delete characters matching a pattern in Elixir include using the `String.delete/2` function or writing a custom function using `String.replace/2` and `Regex.replace/3` to handle more complex patterns.

## See Also:

To learn more about regular expressions in Elixir, check out the [Elixir documentation](https://hexdocs.pm/elixir/Regex.html) on regex. You can also explore the [String](https://hexdocs.pm/elixir/String.html) and [Regex](https://hexdocs.pm/elixir/Regex.html) modules for more information on manipulating strings in Elixir. Other useful resources include online regex testers like [Regex101](https://regex101.com/) and [Rubular](https://rubular.com/) for practicing and testing your regex patterns.