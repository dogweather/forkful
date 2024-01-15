---
title:                "Reading command line arguments"
html_title:           "Fish Shell recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

So, you may have heard about Fish Shell and its capabilities for speeding up your command line workflow. But why would you specifically want to learn about reading command line arguments? Well, it all comes down to efficiency and convenience. By understanding how to use command line arguments in Fish Shell, you can save yourself time and effort by automating repetitive tasks and creating more streamlined workflows.

## How To

Okay, let's dive into the nitty-gritty of how to read command line arguments in Fish Shell. First, we need to understand the basic structure of a command line argument. It consists of two parts: the flag, which is preceded by a dash ("-"), and the value, which follows the flag. For example, in the command `ls -a`, the flag is "a" and there is no value.

To read command line arguments in Fish Shell, we can use the `fish_opt` function. This function takes in two arguments: the flag and the default value. Here's an example:

```
Fish Shell
for arg in (fish_opt -f "") # -f is the flag, "" is the default value
    echo $arg
end
```
This code will loop through all the arguments passed in via command line and print them out. However, it will only print out arguments that have the "f" flag. If there are no arguments with the "f" flag, the default value ("") will be used instead.

To use this in a practical scenario, let's say we want to create a script that will list all files with a certain extension (e.g. ".txt") in the current directory. We can use the `ls` command with the `-f` flag and the extension as the value. Here's the code:

```
Fish Shell
set extension (fish_opt -f .txt)
ls *$extension
```

By using `fish_opt`, we can specify a default value (in this case, ".txt") in case the user doesn't provide a value when running the script. This saves us from having to manually type the extension every time we want to run the script.

## Deep Dive

Now that we have a basic understanding of how to read command line arguments, let's take a deeper look at some other features and functions that can come in handy.

- `count $argv`: This function returns the number of arguments passed in on the command line.
- `commandline -i`: This command prints out all the arguments passed in, including flags and their values.
- `switch case`: By using a `switch` statement, we can specify different actions to take depending on the flag provided in the command line arguments. Here's an example:

```
Fish Shell
switch $arg in
    case -l; echo "Listing files..."
    case -h; echo "Showing help menu..."
end
```

By combining these functions and tools, we can create powerful scripts that utilize command line arguments to their full potential.

## See Also

- [Fish Shell documentation on command line arguments](https://fishshell.com/docs/current/tutorial.html#command-line-arguments)
- [Bash Shell tutorial on command line arguments](https://www.tutorialkart.com/bash-shell-scripting/bash-command-line-arguments/)
- [Command Line Arguments in Python](https://www.geeksforgeeks.org/command-line-arguments-in-python/)