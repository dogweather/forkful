---
title:                "Fish Shell recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer or a system administrator, chances are you've had to deal with command line arguments at some point in your work. While many languages have their own built-in ways of handling them, the Fish Shell offers a convenient and efficient way to read command line arguments without having to write extra code.

## How To

In the Fish Shell, command line arguments are accessed using the ```fish_```` variable. This variable is an array that contains all the arguments passed to the shell when it was launched. To access a specific argument, all you have to do is use the array index starting at 1. Let's see an example:

```Fish Shell
echo $fish_arg[1]
```

Assuming you launched the Fish Shell with the argument "hello", the output of this code block would be "hello". It's that simple!

You can also access all of the arguments at once using the ```fish_args``` array. This can be useful if you want to loop through all the arguments or perform some other operation on the entire list. Let's see an example:

```Fish Shell
for arg in $fish_args
  echo $arg
end
```

This snippet will print out all the arguments passed to the shell, each on a separate line.

## Deep Dive

Behind the scenes, the Fish Shell uses the getopts function to handle command line arguments. This function takes in a list of options and their corresponding variables, and it sets the variables to the values of the options passed to the shell. This allows for more advanced argument handling, such as parsing for specific flags or setting default values for options that are not provided. For more information on using getopts, check out the Fish Shell documentation.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Getting Started with Fish Shell](https://dev.to/nicolasgere/fish-shell-getting-started-53nm)
- [Mastering the Fish Shell](https://medium.com/faun/mastering-the-fish-shell-94681219aef3)