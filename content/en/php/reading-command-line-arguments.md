---
title:                "Reading command line arguments"
html_title:           "PHP recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Have you ever run into a situation where you needed to pass arguments to your PHP script from the command line? Maybe you want to run a script with different options depending on the arguments given. Whatever the case may be, understanding how to read command line arguments in PHP can be a handy tool in your programming arsenal.

## How To

Reading command line arguments in PHP is a simple process. First, we need to access the arguments passed to the script by using the predefined variable `$argv`. This variable is an array that contains all the command line arguments, with the first argument being the name of the script itself.

Let's say we have a script called `greeting.php` and we want to pass in a name and a greeting as arguments. We can do so by running the following command in our terminal:

```
php greeting.php John "Hello, nice to meet you!"
```

Next, we can access these arguments in our script by simply using `$argv[1]` and `$argv[2]` respectively. Let's see an example of how we can use these arguments to output a personalized greeting:

```PHP
// greeting.php

$name = $argv[1]; // "John"
$greeting = $argv[2]; // "Hello, nice to meet you!"

echo "$greeting, $name!"; // "Hello, John!"
```

Running our script with these arguments will output "Hello, John!" on our terminal. We can also add more complex logic to our script to handle different combinations of arguments.

## Deep Dive

In addition to using `$argv`, PHP also provides us with the `argc` variable, which holds the number of arguments passed to the script. This can be useful if we need to perform different actions based on the number of arguments given.

We can also use the `getopt()` function to parse command line options and flags. This is particularly useful when our script needs to handle different options, like `-h` for displaying a help message or `-f` for specifying a file to read from.

There are also third-party libraries, like Symfony's Console component, that can help with more complex command line argument handling. These libraries offer features such as automatic validation and error handling.

## See Also

- [PHP Manual: Handling Command Line Arguments](https://www.php.net/manual/en/features.commandline.arguments.php)
- [Symfony Console component](https://symfony.com/doc/current/components/console.html)
- [Getting Started with getopt() in PHP](https://www.php.net/manual/en/function.getopt.php)