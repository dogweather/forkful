---
title:                "Fish Shell recipe: Reading command line arguments"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to Fish Shell, you might be wondering why you would even bother learning about command line arguments. After all, can't you just click around in your file manager instead? But mastering command line arguments can make your Fish Shell experience more efficient and customizable. 

## How To

To read command line arguments in Fish Shell, you can use the `$argv` variable. This variable contains an array of all the arguments that were passed into the shell. Let's see an example:

```Fish Shell
# command: fish my_script.fish argument1 argument2

echo $argv
```

This will output `argument1 argument2`, showing that the arguments have been stored in the `$argv` array. You can also access individual arguments by using the array index, starting at 1. For example: 

```Fish Shell
echo $argv[1]
```

This will output `argument1`. 

## Deep Dive

While using the `$argv` variable is a quick and easy way to access command line arguments, there are some other useful functions and flags to be aware of. For example, you can use `count` to get the number of arguments passed in, and `contains` to check if a specific argument is present. Additionally, you can use the `-n` flag to only output numeric arguments, or the `-c` flag to only output string arguments. 

## See Also

To learn more about command line arguments in Fish Shell, check out these resources:

- [Fish Shell documentation on Arguments](https://fishshell.com/docs/current/tutorial.html#tut_arguments)
- [The Fish Shell Programming Language](https://fishshell.com/docs/current/index.html#Chapter-tutorial)
- [Guide to Shell Scripting in Fish Shell](https://dev.to/rishirajc/guide-to-shell-scripting-in-fish-shell-3p5m)