---
title:                "Fish Shell recipe: Checking if a directory exists"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

You may find yourself in a situation where you need to check if a specific directory exists before running a task. This could be useful in automating certain processes or to avoid any errors while executing a script.

## How To

Coding in Fish Shell makes it easy to check if a directory exists. Here's an example using the `test` command and the `--directory` flag to check for a directory named "documents":

```Fish Shell
if test -d documents
    echo "The directory exists"
else
    echo "The directory does not exist"
end
```

The above code will first check if the "documents" directory exists using the `test` command and then print a message accordingly. 

Here's another example using the `status` command and the `--is-dir` flag to check if the current working directory is a directory:

```Fish Shell
if status --is-dir
    echo "The current directory is a directory"
else
    echo "The current directory is not a directory"
end
```

The above code will use the `status` command to check the current directory and then print a message based on the result.

## Deep Dive

The `test` command is a built-in command in Fish Shell that allows for evaluating conditions and returning a successful or unsuccessful exit status. Using the `-d` or `--directory` flag will check if the given path is a directory and return a successful exit status if it exists.

The `status` command is also a built-in command that prints information about the given path. The `--is-dir` flag will check if the given path is a directory and print a successful exit status if it is.

## See Also

- Fish Shell Tutorial: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Fish Shell Command Line Tools: [https://fishshell.com/docs/current/cmds.html](https://fishshell.com/docs/current/cmds.html)
- Official Fish Shell Documentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)