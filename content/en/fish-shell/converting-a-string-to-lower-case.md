---
title:                "Fish Shell recipe: Converting a string to lower case"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common task in programming. It allows for efficient and consistent handling of user input, as well as easier comparison between strings. This is especially useful for tasks such as input validation and data processing.

## How To

Converting a string to lower case in Fish Shell is a simple and straightforward process. The `tolower` command is used to convert the string to lower case, as shown in the example below:

```
# Converts the string "Hello World!" to lower case
tolower "Hello World!"
```

This will output `hello world!` in the terminal. Another option is to use the `string` command with the `lower` option, which allows for more control over the output.

```
# Converts the string "Hello World!" to lower case using the string command
string lower "Hello World!"
```

The above code will also output `hello world!`. However, the `string` command allows for additional options, such as specifying a different locale or handling special characters. Refer to the Fish Shell documentation for more information on these options.

## Deep Dive

Underneath the surface, converting a string to lower case involves using the `set` command to assign the string to a variable and then using the `string replace` command to replace all upper case characters with their lower case counterparts. This process is handled by the Fish Shell internally when using the `tolower` and `string` commands, making it an efficient and convenient option for developers.

It is important to note that the `tolower` command does not alter the original variable, but rather creates a new variable with the lower case value. The original variable will remain unchanged unless it is explicitly reassigned.

## See Also

- [Fish Shell documentation on converting strings to lower case](https://fishshell.com/docs/current/index.html#string-expansion-lower)
- [Fish Shell string command basics](https://fishshell.com/docs/current/cmds/string.html)
- [Using Fish Shell's string command for advanced string manipulation](https://www.twilio.com/blog/working-with-text-using-the-fish-shell-string-command)