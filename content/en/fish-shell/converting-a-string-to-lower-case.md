---
title:                "Converting a string to lower case"
html_title:           "Fish Shell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you need to convert a string to lower case in your Fish Shell script? This is a common task for many developers who want to manipulate and compare strings without worrying about case sensitivity. In this article, we will explore how to easily convert a string to lower case in Fish Shell.

## How To

To convert a string to lower case in Fish Shell, we can use the `string tolower` command. This command takes in a string as an argument and returns the same string with all characters converted to lower case. Let's take a look at an example:

```Fish Shell
set my_string "HELLO World"
string tolower $my_string
```

The output of this code would be `hello world`, as all the characters in the original string have been converted to lower case. You can also use the `tolower` shorthand, which achieves the same result:

```Fish Shell
set my_string "HELLO World"
tolower $my_string
```

In both cases, the variable `my_string` remains unchanged, and the converted string is displayed as output. This allows you to both manipulate the string and access the converted version whenever needed.

## Deep Dive

The `string tolower` command in Fish Shell utilizes the `tolower` function, which is a part of the standard library. This function converts a single character or an entire string to lower case, depending on the input. It uses the current locale and encoding to determine the appropriate lower case mapping for each character.

It is worth noting that the `tolower` function does not just convert ASCII characters, but also handles Unicode characters and special cases such as ligatures. This makes it a reliable and robust method for converting strings to lower case in any language.

Furthermore, if you need to convert a string to upper case, you can use the `string toupper` command, which uses the `toupper` function in a similar manner.

## See Also

For more information about string manipulation in Fish Shell, check out the official documentation and these helpful resources:

- [Fish Shell official website](https://fishshell.com)
- [Fish Shell on GitHub](https://github.com/fish-shell/fish-shell)
- [Fish Shell string manipulation tutorial](https://medium.com/@alex_barnsley/fish-shell-string-manipulation-d8d9b5c60a03)