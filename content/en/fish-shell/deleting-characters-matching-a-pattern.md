---
title:                "Deleting characters matching a pattern"
html_title:           "Fish Shell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern, also known as regular expression, is a powerful feature that allows programmers to manipulate text in a specific way. It helps in finding and manipulating multiple instances of characters in any given text, saving time and effort for programmers. 

## How to:

Coding examples in Fish Shell: 

```
# To delete a specific character from a string
set my_string "hello!"
echo $my_string | tr -d 'l'
# Output: heo!

# To delete a range of characters from a string
set my_string "hello!"
echo $my_string | tr -d 'l-o'
# Output: he!

# To delete multiple instances of a character from a string
set my_string "hello!"
echo $my_string | tr -d 'l'
# Output: heo!

# To delete a specific pattern from a string
set my_string "fish_shell_123"
echo $my_string | tr -d '_[0-9]'
# Output: fishshell

```

## Deep Dive:

- **Historical Context**: Regular expressions were first introduced by computer scientist Stephen Cole Kleene in the 1950s as a notation for describing patterns in formal language theory. They were later popularized by Unix tools such as grep, sed, and awk, and have since become an essential tool for text processing.
- **Alternatives**: Other shell languages such as Bash and Zsh also have regular expression capabilities, but the syntax and functions might differ.
- **Implementation details**: Fish Shell uses the tr command for its delete functionality. This command is short for "translate" and is used to delete or change characters in a given string according to the specified pattern.

## See Also:

- [Fish Shell Official Website](https://fishshell.com/): For more information on Fish Shell, including installation and usage guides.
- [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html): A comprehensive tutorial on regular expressions for beginners.
- [Unix Shell Commands](https://www.maths.tcd.ie/~michal/UNIX.html): A list of common Unix shell commands, including tr.