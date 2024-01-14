---
title:    "Fish Shell recipe: Checking if a directory exists"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

If you've ever written a script that interacts with multiple directories, you may have encountered the issue of a missing or nonexistent directory. This can lead to errors and can be frustrating to troubleshoot. By checking if a directory exists before trying to access it, you can prevent these issues and ensure smooth execution of your script.

## How To

To check if a directory exists in Fish Shell, we can use the `test` command with the `-d` flag, which checks if the given path is a directory.

```
if test -d /path/to/directory
    echo "Directory exists!"
else
    echo "Directory does not exist."
end
```

This example checks if the directory at `/path/to/directory` exists and prints an appropriate message. We can also use the `set` command to store the result of the `test` command in a variable.

```
set exists (test -d /path/to/directory; and echo true)
if $exists
    echo "Directory exists!"
else
    echo "Directory does not exist."
end
```

This way, we can use the variable `$exists` to perform other tasks depending on the result of the check. We can also use the `-f` flag with the `test` command to check if a regular file exists at the given path.

## Deep Dive

Under the hood, the `test` command in Fish Shell is actually shorthand for the `fish_test` builtin function. This function uses the `stat` command to determine if the given path exists and is a directory. If we want to check for a symlink, we can use the `-L` flag with the `test` command or call the `fish_stat` function directly.

It's also worth noting that the `test` command in Fish Shell supports multiple checks, such as checking for permissions or file ownership. You can use the `man` command to explore all the available options and flags.

## See Also

- [Fish Shell documentation on `test` command](https://fishshell.com/docs/current/cmds/test.html)
- [Fish Shell documentation on `stat` command](https://fishshell.com/docs/current/cmds/stat.html)
- [Fish Shell documentation on builtins](https://fishshell.com/docs/current/index.html#builtins)