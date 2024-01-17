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

## What & Why?

Reading command line arguments is the process of retrieving user-provided input when using a command line interface. This input can be used to customize the behavior of a program or script. Programmers often use this technique to create more versatile and user-friendly programs that can be modified on-the-go without needing to edit the code.

## How to:

Coding Example:

```
Fish Shell

# This command will display the arguments passed by the user when executing the script.
echo $argv
```

Sample Output:

```
Fish Shell

$ fish shell_example.fish arg1 arg2 arg3
arg1 arg2 arg3
```

If no arguments are provided, the output will be blank. Keep in mind that the `$argv` variable will only contain the arguments passed after the script name, not the script name itself.

Another useful command to retrieve individual arguments is `$argv[1]`, which will display the first argument given by the user. To display all arguments one by one, you can use a for loop:

```
Fish Shell

# This loop will display each argument on a separate line.
for arg in $argv
    echo $arg
end
```

Sample Output:

```
Fish Shell

$ fish shell_example.fish arg1 arg2 arg3
arg1
arg2
arg3

```

## Deep Dive

Command line arguments have been an integral part of shell programming since the earliest days of Unix. They allow programs to be tailored to specific needs without the need for hardcoded values.

While the `$argv` variable is specific to the Fish shell, other shells have their own equivalent variables. For example, in Bash, the command line arguments can be accessed through the `$@` variable, while in Zsh, they can be accessed through the `$args` array.

There are also alternative ways to parse command line arguments, such as using the `getopt` system utility in conjunction with a `while` loop, or using a scripting language like Python or Ruby.

## See Also

- [Fish Shell's Official Documentation on Command Substitutions](https://fishshell.com/docs/current/cmdsubst.html)
- [Bash's Official Documentation on Command Line Arguments](https://www.gnu.org/software/bash/manual/html_node/Command-Line-Arguments.html)
- [Zsh's Official Documentation on Command Substitutions](https://zsh.sourceforge.io/Doc/Release/Expansion.html#Command-Substitution)
- [Alternative methods for parsing command line arguments](https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash)