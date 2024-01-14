---
title:    "Fish Shell recipe: Checking if a directory exists"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Why

Checking if a directory exists may seem like a simple task, but it can actually be quite useful in various situations. For example, it can help with error handling in your scripts or ensure that certain directories are in place before proceeding with a task. 

# How To

## Using the `test` command
 
The `test` command is a built-in command in Fish Shell that can be used to evaluate conditions. This includes checking if a directory exists. Here's an example:

```
test -d ~/Documents
```

In this example, we are using the `-d` option to check if the `Documents` directory exists in our home directory. If the directory exists, the command will return a status code of `0`, indicating success. If the directory does not exist, the command will return a status code of `1`, indicating failure. 

## Using the `if` statement

Another way to check if a directory exists is by using the `if` statement. Here's an example:

```
if test -d ~/Documents
    echo "Documents directory exists!"
else
    echo "Documents directory does not exist."
end
```

In this example, we are using the same `test` command within the conditional statement. If the directory exists, the first block of code will be executed, and if not, the second block of code will be executed. This is helpful for error handling or for performing certain tasks only if the directory exists. 

# Deep Dive

The `test` command actually has a variety of options that can be used for checking different types of files or directories. Some useful options for directories include:

- `-d`: checks if the file is a directory
- `-e`: checks if the file exists
- `-L`: checks if the file is a symbolic link
- `-x`: checks if the file is executable

These options can be combined with other commands or used within conditional statements to perform more complex checks. 

# See Also

- Fish Shell documentation on `test` command: https://fishshell.com/docs/current/commands.html#test
- More examples of using `test` command for checking different file types: https://fishshell.com/docs/current/tutorial.html#tutorial-conditions