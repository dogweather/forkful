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

## What & Why?

Regular expressions (regex) are sequences of characters used for pattern matching in strings. Programmers use regex to find, replace, and manage text efficiently.

## How To:

In Elixir, Regex module is used for regex operations. Below are some examples:

Matching:

```elixir
# Check if the string has "elixir" in it
Regex.match?(~r/elixir/, "Programming in Elixir is fun!") 
# Output: true
```

Replacing:

```elixir
# Replace "elixir" with "Elixir!" 
Regex.replace(~r/elixir/, "Programming in elixir is fun!", "Elixir!")
# Output: "Programming in Elixir! is fun!"
```

Capturing:

```elixir
# Retrieve 'elixir' from the string
Regex.run(~r/elixir/, "Programming in elixir is fun!") 
# Output: ["elixir"]
```

## Deep Dive

Regex appeared in the 1950s, in theoretical computer science. Yet, its practical realization came into light in Ken Thompson's implementation for the Unix editor QED.

Elixir's Regex builds upon the PCRE library (Perl Compatible Regular Expressions). This offers many advanced regex features, but also brings its potential complexities that may overcomplicate simple tasks. 

For simpler operations, `String` module’s inbuilt functions could be an ideal alternative. For example, detecting a substring can be conveniently handled like so:

```elixir
# Does the string contain "elixir"?
String.contains?("Programming in Elixir is fun!", "elixir")
# Output: true
```

## See Also

- [Official Elixir Docs on Regex](https://hexdocs.pm/elixir/Regex.html)
- [Elixir School’s Regex Guide](https://elixirschool.com/en/lessons/advanced/regular-expressions/)
- [Perl Compatible Regular Expressions (PCRE) Documentation](http://www.pcre.org/original/pcre.txt)