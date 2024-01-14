---
title:    "Fish Shell recipe: Printing debug output"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the programming process. It helps to identify and fix issues in code so that it can run smoothly. One way to debug is by printing out specific values at different points in the code to see how it is behaving. This can be done with the use of the `echo` command in Fish Shell.

## How To

To print out debug output in Fish Shell, simply use the `echo` command followed by the value or variable you want to print. For example:

```Fish Shell
echo "Hello world"
```

This will print out the string "Hello world" in the console when the code is executed.

You can also use `echo` to print out the values of variables. For example:

```Fish Shell
set my_variable "debug"
echo $my_variable
```

This will print out the value of the variable `my_variable`, which in this case is "debug". This can be useful when trying to track the value of a variable throughout the code.

## Deep Dive

There are a few different options for printing debug output in Fish Shell. One option is to use the `-v` flag with the `set` command. This will print out not only the value of the variable, but also the name of the variable.

```Fish Shell
set -v my_variable "debug"
```

This will print out `my_variable=debug` in the console.

Another option is to use the `printf` command. This is useful for printing out more complex output, such as combining strings with variables.

```Fish Shell
set my_name "John"
printf "Hello, my name is %s\n" $my_name
```

This will print out `Hello, my name is John` in the console.

It is also possible to redirect the output of `echo` or `printf` to a file using the `>` symbol. This can be helpful for saving debug output for later analysis.

## See Also

- [Fish Shell documentation on `echo`](https://fishshell.com/docs/current/cmds/echo.html)
- [Fish Shell documentation on `set`](https://fishshell.com/docs/current/cmds/set.html)
- [Fish Shell documentation on `printf`](https://fishshell.com/docs/current/cmds/printf.html)