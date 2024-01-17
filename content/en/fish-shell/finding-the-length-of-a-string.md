---
title:                "Finding the length of a string"
html_title:           "Fish Shell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string is the process of determining the number of characters in a given string. Programmers often need to know the length of a string in order to perform manipulations or comparisons on it within their code.

## How to:
Finding the length of a string is a simple task in the Fish Shell. You can use the built-in `string` command with the `-l` flag to display the length of a given string.

```Fish Shell
string length "Hello World"
```

This will output `11`, as there are 11 characters in the string "Hello World". You can also use this command within a variable to store the length for later use:

```Fish Shell
set mystring "This is a test"
set length (string length $mystring)
echo $length
```

This will output `14`, as there are 14 characters in the string "This is a test".

## Deep Dive:
The concept of finding the length of a string has been around since the early days of programming. In older programming languages, this was often a complex and time-consuming task. However, with modern languages like Fish Shell, finding the length of a string is a simple and efficient process.

There are also other ways to find the length of a string in Fish Shell, such as using the `len` function or the `count` command. However, the `string` command with the `-l` flag remains the most commonly used method.

## See Also:
- [Fish Shell string command documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell len function documentation](https://fishshell.com/docs/current/cmds/string.html#len)
- [Fish Shell count command documentation](https://fishshell.com/docs/current/cmds/count.html)