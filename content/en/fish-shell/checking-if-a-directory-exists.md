---
title:                "Checking if a directory exists"
html_title:           "Fish Shell recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a way for programmers to determine whether a specific directory exists within the system. This is useful for tasks such as creating a new directory only if it does not already exist, or for checking if a certain file path is valid before performing any actions on it.

## How to:

To check if a directory exists in Fish Shell, we can use the `test` command with the `-d` flag. For example:
```
Fish Shell> test -d /home/user/directory
```
This will return a `0` exit code if the directory exists, or a `1` exit code if it does not.

We can also use the `if` statement to check for the existence of a directory in a more readable way:
```
Fish Shell> if test -d /home/user/directory
                 echo "Directory exists!"
             else
                 echo "Directory does not exist."
             end
```

## Deep Dive:

In the early days of shell programming, the `test` command was known as `[` and required a closing `]` bracket. This syntax can still be used for POSIX compatibility. Some other shells may also have a dedicated `test` command, but in Fish Shell, it is built in.

As an alternative to using the `test` command, we can also use the `test` function provided by Fish Shell. This has the advantage of being more readable and easier to use, but it is not POSIX compatible.

Implementation wise, the `test` command checks if the given file path exists and if it is a directory. This is done using the `stat` system call. If the file exists and is a directory, it will return a `0` exit code, otherwise it will return a `1` exit code.

## See Also:

- [Fish Shell documentation on `test`](https://fishshell.com/docs/current/cmds/test.html)
- [A tutorial on File and Directory Testing in Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)