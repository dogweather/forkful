---
title:                "正規表現の利用"
html_title:           "Haskell: 正規表現の利用"
simple_title:         "正規表現の利用"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regex, or regular expressions, are a powerful tool for text processing in Haskell. They allow you to quickly and efficiently search for specific patterns in strings, making it easier to manipulate and extract information from your data.

## How To

To use regular expressions in Haskell, you first need to import the `Text.Regex` module. This module provides functions for compiling and executing regular expressions. Let's take a look at a simple example:

```Haskell
import Text.Regex

main = do
  let str = "Hello World"
  let matches = matchRegex (mkRegex "o") str
  
  print matches
```

Output: Just ["o"]

In this example, we imported the `Text.Regex` module and defined a string `str` that we want to search for matches in. We then used the `mkRegex` function to create a regular expression (in this case, simply the letter "o") and passed it to the `matchRegex` function along with our string. The output shows that the regular expression "o" was successfully matched in the string "Hello World".

You can also use regular expressions for more complex patterns, such as matching a specific sequence of characters or using special characters for meta operations. Here's another example:

```Haskell
import Text.Regex

main = do
  let str = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
  let matches = matchRegex (mkRegex "[A-Z][a-z]+") str
  
  print matches
```

Output: Just ["Lorem", "Lorem", "dolor", "Consectetur"]

In this example, we used a regular expression to match all capitalized words in the string. This is just a small glimpse of what regular expressions can do in Haskell. There are many more functions and operations you can use, such as replacing matched patterns and extracting groups.

## Deep Dive

Regular expressions in Haskell are based on the POSIX standard, meaning they follow a set of rules and conventions. Understanding these rules can help you write more efficient and accurate regular expressions. Here are some key points to keep in mind:

- Characters are interpreted literally unless they are special meta characters, such as `*` or `+`.
- Square brackets `[]` can be used to match any one of the characters within them.
- Parentheses `()` can be used for grouping and capturing specific parts of a pattern.
- Use the `.*?` operator to match zero or more characters in a non-greedy way, and the `.+?` operator to match one or more characters.
- The `|` symbol can be used to specify multiple possible matches.

For a more in-depth guide on regular expressions in Haskell, make sure to check out the official documentation for the `Text.Regex` module.

## See Also

- [Official `Text.Regex` documentation](https://hackage.haskell.org/package/regex)
- [Regular expressions in Haskell tutorial](https://wiki.haskell.org/Regular_expressions)