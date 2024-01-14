---
title:    "PHP recipe: Reading command line arguments"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, it's important to have the ability to manipulate your program based on different inputs from users or external sources. One way to achieve this is through the use of command line arguments. By reading these arguments, your program can take different paths and produce different outputs, increasing its versatility and usefulness.

## How To

To read command line arguments in PHP, we can use the `$_SERVER['argv']` variable. This variable contains an array of all the arguments passed in when executing the program. Let's look at an example:

```PHP
<?php
// file: command_line_arguments.php
$args = $_SERVER['argv'];

// print out all the arguments
foreach ($args as $arg) {
    echo $arg . "\n";
}
```

If we execute this file in the command line by typing `php command_line_arguments.php hello world`, the output will be:

```
hello
world
```

We can also access individual arguments by indexing the `$_SERVER['argv']` array. For example, if we want to print out only the second argument, we can use `$args[1]` since the first argument is at index 0.

You can also use flags and options in command line arguments, which are preceded by a dash (`-`). These can be stored in a separate array called `$_SERVER['argv']`.

## Deep Dive

In addition to the regular command line arguments, there are also special arguments that can be used. These include `--help`, which displays the program's instructions, and `-h`, a shortened version of `--help`.

We can also use the `getopt()` function in PHP to handle command line options and flags. This function takes in the options and flags as well as any required arguments and returns them in an associative array. Let's look at an example:

```PHP
<?php
// file: command_line_getopt.php
$options = getopt("f:l:");

// if the -f flag is used, print the file name
if (isset($options['f'])) {
    echo "File name: {$options['f']}\n";
}

// if the -l flag is used, print the last name
if (isset($options['l'])) {
    echo "Last name: {$options['l']}\n";
}
```

If we execute this file in the command line by typing `php command_line_getopt.php -f index.php -l Smith`, the output will be:

```
File name: index.php
Last name: Smith
```

## See Also

For more information on command line arguments in PHP, check out the following links:

- [PHP Command Line Arguments](https://www.php.net/manual/en/reserved.variables.argv.php)
- [getopt() Function](https://www.php.net/manual/en/function.getopt.php)
- [Command Line Arguments in PHP](https://www.tutorialspoint.com/php/php_command_line.htm)