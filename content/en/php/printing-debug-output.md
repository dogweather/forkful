---
title:                "PHP recipe: Printing debug output"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an inevitable part of programming. As developers, we often come across errors and bugs in our code that need fixing. In such situations, printing debug output can be a very useful tool. It allows us to view what is happening in our code and helps us identify the cause of the problem.

## How To
Printing debug output in PHP is quite simple. In this example, we will be using the `echo` statement to print our debug output.

```
<?php
$variable = "Hello World!";
echo $variable;
```

This code will output `Hello World!` to the screen. But what if we want to print out the value of a variable during runtime? That's when `echo` becomes extremely handy.

```
<?php
$variable = "Hello World!";
echo "The value of the variable is: " . $variable;
```

The output of this code will be `The value of the variable is: Hello World!` This allows us to see the value of our variable at that specific point in our code. We can also use `var_dump()` to get even more detailed information about our variables, such as their data type and size.

```
<?php
$variable = "Hello World!";
var_dump($variable);

/* Output:
string(12) "Hello World!"
*/
```

## Deep Dive
There are various other functions and methods that can be used for printing debug output in PHP. Some commonly used functions are `print_r()`, `debug_print_backtrace()`, and `error_log()`. These functions can be very useful when trying to debug complex code.

In addition, many integrated development environments (IDEs) offer their own debuggers, which allow developers to step through their code and view the values of variables at each step.

It's also important to note that printing debug output should not be used as a permanent solution. It's best to remove any debug statements before deploying your code to avoid any unnecessary output.

## See Also
- https://www.php.net/manual/en/function.echo.php
- https://www.php.net/manual/en/function.var-dump.php
- https://www.php.net/manual/en/function.error-log.php