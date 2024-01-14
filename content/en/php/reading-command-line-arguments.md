---
title:                "PHP recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Reading command line arguments is an essential skill for any PHP programmer. It allows for more flexibility and control over how your program runs, making it a valuable tool to have in your coding arsenal.

## How To
Reading command line arguments in PHP is a straightforward process. First, we use the `$_SERVER['argv']` variable to get an array of the command line arguments. This array includes the name of the script itself as the first item, followed by any additional arguments passed in.

To access these arguments, we can use the `argv` array just like any other array in PHP. For example, if we wanted to access the second argument, we would use `$_SERVER['argv'][1]`.

Let's look at an example of how to use command line arguments in PHP. Say we want to create a program that takes in two numbers as arguments and multiplies them together. Our code would look like this:

```PHP
$num1 = $_SERVER['argv'][1];
$num2 = $_SERVER['argv'][2];

$result = $num1 * $num2;

echo "The result is: $result";
```

If we were to run this program by typing `php multiply.php 5 10` into the command line, our output would be:

```
The result is: 50
```

## Deep Dive
One important thing to keep in mind when working with command line arguments is that they are always passed in as strings. Therefore, if we want to use the arguments as integers, we would need to use the `intval()` function to convert them.

It's also worth noting that command line arguments can be passed in any order. However, if we want to pass in arguments with specific names, we can use the `getopt()` function. This function allows us to define named arguments and their values, making it easier to work with specific inputs.

For more advanced use cases, we can also use the `getopt()` function to define optional flags and arguments. This allows for more flexibility in our programs and makes them more user-friendly.

## See Also
For more information on reading command line arguments in PHP, check out the official PHP documentation:
- https://www.php.net/manual/en/reserved.variables.argv.php
- https://www.php.net/manual/en/function.getopt.php

Happy coding!