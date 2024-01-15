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

## Why

If you're a developer or a system administrator, you may often encounter situations where you need to check if a certain directory exists before running a command. This is important for preventing errors and ensuring smooth execution of your scripts.

## How To

To check if a directory exists in Fish Shell, you can use the `test` command with the `-d` flag. This flag checks if the given path is a directory.

```
Fish Shell
if test -d <directory_path>
    echo "Directory exists!"
end
```

If the directory exists, the `echo` statement will be executed, and if it doesn't, nothing will happen.

You can also use the `and` and `or` operators to perform operations based on the existence of the directory.

```
Fish Shell
if test -d <directory_path> and <condition>
    # do something
else if test -d <directory_path> or test -d <another_directory_path>
    # do something else
end
```

This can come in handy when you want to execute different commands based on the existence of different directories.

## Deep Dive

The `test` command in Fish Shell is a built-in shell command and is also known as the `[` command. It has various flags for performing different types of tests, including checking the existence of a directory.

In Fish Shell, the keyword `test` is equivalent to the `[` command with the `-f` flag, which checks for the existence of a file. This is why we need to explicitly specify the `-d` flag to check for the existence of a directory.

You can also use the `set -q` command to make the code snippet more concise. This command returns `1` if the given path exists, and `0` if it doesn't.

```
Fish Shell
if set -q <directory_path>
    # do something
end
```

With this, you don't need to specify the `-d` flag, as it automatically checks for the existence of a directory.

## See Also

- [Fish Shell documentation on `test` command](https://fishshell.com/docs/current/cmds/test.html)
- [Fish Shell tutorial on conditional statements](https://fishshell.com/docs/current/tutorial.html#conditional-statements)