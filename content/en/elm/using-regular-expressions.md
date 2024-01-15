---
title:                "Using regular expressions"
html_title:           "Elm recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

If you're new to programming, you may not be familiar with regular expressions. These powerful tools allow you to search and manipulate strings of text with just a few lines of code. Using regular expressions can save you time and energy by streamlining your code and making complex text processing tasks easier.

## How To

Using regular expressions in Elm is simple and straightforward. First, you'll need to import the standard library for regular expressions by adding `import Regex` to the top of your code. Then, you can use the `Regex.fromString` function to create a regular expression pattern. For example:

```elm
import Regex

pattern = Regex.fromString "[a-z]+"
```

This code creates a regular expression that will match any lowercase letters in a string. Next, you'll need to use the `Regex.find` or `Regex.replace` function to search for or manipulate strings using your pattern. For example:

```elm
string = "Hello regular expressions!"

matches = Regex.find pattern string

-- matches = ["ello","regular","expressions"]
```

Here, we use `Regex.find` to search for any matches within our string and return the results as a list. You can also use `Regex.replace` to replace any matches with a specified string.

## Deep Dive

Regular expressions offer a wide range of powerful tools for string manipulation. You can use special characters and symbols to search for patterns such as digits, whitespace, and specific characters. Additionally, you can use modifiers to find variations or repetitions of these patterns. For a full list of available characters and modifiers, check out the [regular expressions documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).

It's important to note that regular expressions can be complex and difficult to read, especially for beginners. Make sure to test and debug your expressions thoroughly to ensure they are working as intended. You can also use online tools, such as [regex101](https://regex101.com/), to help you build and test your expressions.

## See Also

For more information on using regular expressions in Elm, check out the official [documentation](https://package.elm-lang.org/packages/elm/regex/latest/) and this [tutorial](https://survivejs.com/elm/parsing/regular-expressions/). Also, consider learning about [functional programming in Elm](https://www.chrisjmendez.com/2016/03/02/functional-programming-with-elm/), as it will help you better understand and utilize regular expressions.