---
title:                "Using regular expressions"
html_title:           "Elixir recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful tool for pattern matching and text manipulation in Elixir. They allow developers to easily search for specific patterns in strings and perform various operations based on those patterns. This can save time and make code more efficient.

## How To

To use regular expressions in Elixir, we first need to import the `Regex` module. Let's take a look at some simple examples:

```
Elixir: Match a string and extract a variable:

rx = ~r/Hello, (.*?)/
"Hello, Alice" =~ rx
# Output: {["Hello, Alice"], ["Alice"]}

Elixir: Replace all occurrences of a pattern:

rx = ~r/dog/
String.replace("I have a cat and a dog", rx, "horse")
# Output: "I have a cat and a horse"
```

In the first example, we match the string "Hello, Alice" against the regular expression `~r/Hello, (.*?)/` which looks for the word "Hello," followed by any characters. The match returns a tuple with the full match and the extracted variable, which in this case is "Alice".

In the second example, we use `String.replace/3` to replace all instances of the word "dog" with "horse" in the given string.

Regular expressions also have various modifiers and special characters that allow for more complex pattern matching. These include `^` for matching the beginning of a string, `$` for matching the end of a string, and `+` for matching one or more occurrences of the previous pattern.

## Deep Dive

One of the most useful aspects of regular expressions in Elixir is their ability to reference capture groups. These are portions of the matched string that are enclosed in parentheses. We can then use these capture groups in our replacements by using the backslash followed by the group number (`\1` for the first group, `\2` for the second, and so on).

```
rx = ~r/(\w+), (\w+)/
"John, Smith" =~ rx
# Output: {["John, Smith"], ["John", "Smith"]}

# Using capture groups for replacement
String.replace("John, Smith", rx, "\\2, \\1")
# Output: "Smith, John"
```

We can also make our regular expressions more specific by using character classes, which are enclosed in square brackets (`[]`). For example, `[aeiou]` will match any vowel, allowing us to easily search for specific letters or patterns within strings.

## See Also

- [Elixir's Regex module documentation](https://hexdocs.pm/elixir/Regex.html)
- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
- [Regex101 - an online regular expression tester](https://regex101.com/)