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

## Why

Regular expressions are a powerful tool for string manipulation and pattern matching in the Fish Shell. They allow you to search, replace, and extract text in a flexible and efficient manner. By learning how to use regular expressions, you can save time and streamline your coding process.

## How To

Using regular expressions in the Fish Shell is simple. You can start by using the `grep` command to search for a specific pattern in a file or command output. For example, let's say we have a file called `names.txt` containing a list of names:

```
John
Emily
Michael
Jane
```

We can use regular expressions to search for all names that start with "J" by using the following command:

```
grep ^J names.txt
```

The `^` symbol represents the beginning of a line, so this command will return "John" and "Jane" as results.

To replace text with regular expressions, we can use the `sed` command. For instance, if we want to change all "e" characters to "E" in `names.txt`, we can use the following command:

```
sed -i 's/e/E/g' names.txt
```

The `-i` flag ensures that the changes are made directly in the file and the `g` flag makes the replacement global, meaning all instances of "e" will be replaced with "E".

## Deep Dive

Regular expressions have a wide range of symbols and operators that allow for complex pattern matching. Here are some useful resources to learn more:

- [Fish Shell documentation on regular expressions](https://fishshell.com/docs/3.1/cmds/grep.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - an in-depth guide on regular expressions
- [Regex101](https://regex101.com/) - an online tool to test and learn regular expressions

See Also

- [Fish Shell: Getting Started](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell: Command Substitution](https://fishshell.com/docs/current/tutorial.html#builtin-command-substitution) - another useful feature in the Fish Shell
- [Fish Shell: Pipes and Redirections](https://fishshell.com/docs/current/tutorial.html#pipes-and-redirections) - learn how to use pipes and redirections with regular expressions.