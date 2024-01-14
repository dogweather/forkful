---
title:                "PHP recipe: Reading command line arguments"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often encounter the need to make our code more flexible and customizable. One way to do this is by allowing the user to input arguments when running our code. This not only makes our code more interactive, but also saves us from hardcoding certain values. In this blog post, we will explore how to read command line arguments in PHP and why it's beneficial for our code.

## How To

Reading command line arguments in PHP is fairly simple. First, we need to use the `$argv` variable, which is an array containing all the arguments passed to our script when it is executed from the command line. The first element of this array is always the path to the executed script. Let's see an example:

```PHP
<?php

// demo.php
echo "Hello, " . $argv[1] . "!";
```

Now, if we run the script from the command line with an argument, like this: `php demo.php John`, the output will be: `Hello, John!`. This is because `John` is passed as the second element in the `$argv` array (the first being `demo.php`).

We can also use the `$argc` variable to get the number of arguments passed. In our example above, `$argc` would be 2 since we have two arguments, including the script name.

Apart from simple strings, we can also read in more complex data types like arrays and JSON objects. The arguments passed from the command line are always received as strings, so we need to use functions like `explode()` or `json_decode()` to convert them into the desired data type.

## Deep Dive

Now that we know how to read command line arguments, let's dive deeper into why it's beneficial for our code. As mentioned earlier, reading command line arguments makes our code more flexible and customizable. It allows us to change the behavior of our code based on user input without having to modify the code itself. This is especially useful for scripts that are meant to be run multiple times with different input.

Another benefit is that it saves us the time and effort of manually changing values in the code every time we want to test a different scenario. We can simply pass in different arguments and get the desired output without having to modify the code each time.

Additionally, reading command line arguments can also make our code more user-friendly. For example, if we have a script that performs a specific task on a file, we can pass the file name as an argument, making it easier for the user to specify which file they want the script to work on.

Overall, reading command line arguments enhances the functionality and user experience of our code. It's a simple yet powerful tool that can be used in many different scenarios.

## See Also

- [PHP Documentation - $argv](https://www.php.net/manual/en/reserved.variables.argv.php)
- [PHP Documentation - $argc](https://www.php.net/manual/en/reserved.variables.argc.php)
- [GeeksforGeeks - Reading command line arguments in PHP](https://www.geeksforgeeks.org/reading-command-line-arguments-in-php/)
- [Medium - How to read command line arguments in PHP](https://medium.com/@liorohen/how-to-read-command-line-arguments-in-php-91c93b8ac5c5)