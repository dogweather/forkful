---
title:                "Elixir recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions may seem intimidating at first, but once you understand their power, they become an essential tool in your Elixir programming arsenal. With regular expressions, you can easily manipulate strings and perform complex pattern matching, making your code more efficient and concise.

## How To

To use regular expressions in Elixir, you first need to import the `Regex` module. You can do this by adding the `use Regex` statement at the top of your file. Once imported, you can use the `~r` sigil to define regular expressions.

```Elixir
import Regex

# Defining a regular expression
regex = ~r/foo/

# Matching a string against the regex
"foo bar" =~ regex
# Output: true
```

You can also use the `~r` sigil with options to specify more complex patterns. For example, you can use the `i` option to make the regex case-insensitive.

```Elixir
# Defining a case-insensitive regex
regex = ~r/foo/i

# Matching a string against the regex
"foO BAR" =~ regex
# Output: true
```

You can also use regular expressions with the `Regex.match?/2` function to extract specific parts of a string.

```Elixir
# Defining a regex with capture groups
regex = ~r/(foo) (bar)/

# Matching a string and extracting the captured groups
{"foo bar", match} = Regex.match?("foo bar", regex)

# Accessing the captured groups
match["1"] 
# Output: "foo"
match["2"]
# Output: "bar"
```

## Deep Dive

Regular expressions in Elixir follow the same syntax as those in other programming languages, with a few minor differences. Elixir uses the PCRE (Perl Compatible Regular Expressions) library, which is a popular and widely-used library for regular expressions. This means that you can use any PCRE syntax in your Elixir regular expressions, including lookaheads, lookbehinds, and backreferences.

Another key feature of regular expressions in Elixir is the ability to use named capture groups. Instead of accessing captured groups by their index, you can use a name to retrieve the data. This provides more readability and allows you to reference the captured group by its purpose instead of its position.

```Elixir
# Defining a regex with named capture groups
regex = ~r/(?<first_name>John) (?<last_name>Doe)/

# Matching a string and extracting the named capture groups
{"John Doe", match} = Regex.match?("John Doe", regex)

# Accessing the captured groups by name
match["first_name"]
# Output: "John"
match["last_name"]
# Output: "Doe"
```

## See Also

- Official Elixir documentation for regular expressions: https://hexdocs.pm/elixir/Regex.html
- Regex tutorial for Elixir: https://elixirschool.com/en/lessons/advanced/regex/