---
title:    "PHP recipe: Printing debug output"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the programming process, and one of the most effective ways to troubleshoot issues is by printing out the values of variables during runtime. Seeing the exact values can help pinpoint the source of the problem and make it easier to fix.

## How To
To print debug output in PHP, the most commonly used method is the `var_dump()` function. It displays the structure and data type of a given variable or expression and is useful for debugging complex objects and arrays. Here's an example:

```PHP
$name = "John";
$age = 25;
var_dump($name, $age);
```

The above code would output the following:

```
string(4) "John"
int(25)
```

For more concise output, you can use the `print_r()` function, which displays the contents of an array or object in a more readable format. Here's an example:

```PHP
$numbers = [1, 2, 3];
print_r($numbers);
```

The output would look like this:

```
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Another useful function for debugging is `var_export()`, which outputs variables and arrays in a format that can be copied and pasted back into a script. Here's an example:

```PHP
$fruit = "apple";
$colors = ["red", "green", "yellow"];
var_export($fruit, $colors);
```

The output would be:

```
'apple'array (
  0 => 'red',
  1 => 'green',
  2 => 'yellow',
)
```

Aside from these functions, you can also use `error_log()` to print debug output to a log file or `die()` to immediately stop the script execution and show the output on the page.

## Deep Dive
Apart from the functions mentioned above, there are also other ways to print debug output in PHP. You can use `echo` or `printf` to output custom messages and variables in a more controlled manner. Another handy tool is the `debug_backtrace()` function, which displays information about the current call stack and can be useful for tracing the flow of execution.

It's also worth noting that debugging output can be disabled in production environments for security and performance reasons. To prevent the printing of debug information, you can use `ini_set('display_errors', 0)` or `error_reporting(0)`.

## See Also
- [PHP var_dump() function](https://www.php.net/manual/en/function.var-dump.php)
- [PHP print_r() function](https://www.php.net/manual/en/function.print-r.php)
- [PHP var_export() function](https://www.php.net/manual/en/function.var-export.php)
- [PHP error_log() function](https://www.php.net/manual/en/function.error-log.php)
- [PHP debug_backtrace() function](https://www.php.net/manual/en/function.debug-backtrace.php)