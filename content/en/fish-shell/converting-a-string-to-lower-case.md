---
title:                "Fish Shell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case may seem like a simple task, but it can make a big difference in your Fish Shell programming. It allows for more consistency in data and can also make comparing and sorting strings easier.

## How To
To convert a string to lower case in Fish Shell, you can use the `lowercase` command. Let's take a look at an example:

```Fish Shell
# Assign a string to a variable
set my_string "HELLO WORLD"

# Use the lowercase command to convert to lower case
lowercase $my_string

# Output:
hello world
```

You can also use the `string` command with the `to-lower` subcommand to achieve the same result:

```Fish Shell
# Assign a string to a variable
set my_string "HeLlO WoRlD"

# Use the string command with to-lower subcommand
string to-lower $my_string

# Output:
hello world
```

Both of these methods will convert the string to all lowercase letters. However, if you only want to convert the first letter of the string to lowercase, you can use the `string` command with the `to-lower` subcommand and the `--capitalize` option:

```Fish Shell
# Assign a string to a variable
set my_string "HELLO WORLD"

# Use the string command with to-lower and capitalize options
string to-lower --capitalize $my_string

# Output:
Hello world
```

## Deep Dive
Now, let's take a deeper look at why and how these commands work. The `lowercase` and `string to-lower` commands both use a built-in function called `to-lower`. This function takes in a string as an argument and returns a new string with all lowercase letters. It uses the `tolower()` function from the C standard library to achieve this.

Additionally, the `string` command has the ability to perform string manipulations and transformations, including converting strings to lower or upper case. The `to-lower` and `to-upper` subcommands use the `to-lower` and `to-upper` functions respectively to convert strings.

## See Also
- Fish Shell documentation on the `lowercase` command: https://fishshell.com/docs/current/cmds/lowercase.html
- Fish Shell documentation on the `string` command: https://fishshell.com/docs/current/cmds/string.html