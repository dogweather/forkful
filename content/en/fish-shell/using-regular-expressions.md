---
title:                "Using regular expressions"
html_title:           "Fish Shell recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Using regular expressions in programming is a way to search and manipulate text using a set of patterns and rules. It allows programmers to quickly find and extract specific data from a large amount of text, making it a powerful tool for tasks such as data validation, text processing, and data scraping. Regular expressions can significantly increase efficiency and accuracy when working with text-based data.

## How to:

Coding with regular expressions in Fish Shell is done using the built-in `string` command. Here are a few examples of how it can be used:

```
# Match a specific pattern in a string
string match "word" "This is a sentence with the word hello"
# Output: hello
```

```
# Find all instances of a pattern in a string
string match -a "a" "aaaaa"
# Output: a a a a a
```

```
# Replace a pattern in a string with another string
string replace "word" "replacement" "This is a sentence with the wrong word"
# Output: This is a sentence with the wrong replacement
```

## Deep Dive:

The use of regular expressions can be traced back to the 1950s when mathematician Stephen Cole Kleene introduced the concept in formal language theory. However, it was not until the 1980s that it gained popularity in computer programming.

Although regular expressions can be written in various languages, using them in Fish Shell allows for a more concise and simple syntax. Additionally, Fish Shell's `string` command is optimized for performance, making it a reliable and efficient option for regular expression tasks.

Alternatives to using regular expressions in Fish Shell include using built-in string methods or external packages such as `grep` or `sed`. However, these alternatives may not have the same level of functionality and flexibility as regular expressions.

Another interesting aspect of using regular expressions in Fish Shell is the ability to define your own custom aliases and functions. This can help simplify complex regular expressions and make them easier to use in your code.

## See Also:

- [Fish Shell Official Documentation on Regular Expressions](https://fishshell.com/docs/current/commands.html#string)
- [Regular Expression 101 - Online regex tester and debugger](https://regex101.com/)
- [Introduction to Regular Expressions - Tutorial by Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)