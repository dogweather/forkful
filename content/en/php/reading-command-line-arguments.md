---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in PHP is a way to input data right from the system's terminal/command prompt. It's crucial for developers to write scripts that can take dynamic input and behave accordingly.

## How to:

To access command line arguments in PHP, we use the `argc` and `argv` variables.

`argc` determines the length of the array (the number of arguments), and `argv` is an array containing the arguments. Here is a basic example:

```PHP
#!/usr/bin/php
<?php
    //printing the number of arguments
    echo "Number of arguments: " . $argc . "\n";

    //looping through arguments
    foreach ($argv as $arg) {
        echo $arg . "\n";
    }
?>
```

In a terminal, you could run this as `php script.php arg1 arg2 arg3` and get:

```
Number of arguments: 4
script.php
arg1
arg2
arg3
```

This shows the arguments being passed into the PHP script from the command line.

## Deep Dive

While reading command line arguments is only one way PHP interacts with the system, it's a method that's been around since PHP 4 (we're now in PHP 8) so it's worth understanding.

Looking at alternatives, there are libraries such as Symfony's Console Component, but they might be overkill for simpler tasks. The choice largely depends on the complexity of your needs.

As for the details, PHP receives these args via the global variables `$argc` and `$argv` automatically. It's important to note that these are not available when running a script with a web server. They are only defined when the script is run from a command line interface (CLI).

## See Also

For more information on command line arguments in PHP, look at the official documentation on the [PHP site](https://www.php.net/manual/en/reserved.variables.argv.php). Check out this [StackOverflow thread](https://stackoverflow.com/questions/1921421/get-the-application-executing-file) for popular use cases. You might also find Symfony's [Console Component](https://symfony.com/doc/current/components/console.html) useful when dealing with more complex scenarios.