---
title:                "Printing debug output"
html_title:           "PHP recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the development process. It involves identifying and fixing errors in code, which can be a frustrating and time-consuming task. One way to make debugging more efficient is by utilizing print-debugging, which involves printing out specific values and messages to help identify where the issue lies in the code.

## How To

Printing debug output in PHP is a straightforward process. All you need is the `echo` or `print` statement, followed by the variable or message you want to display. Let's look at a simple example:

```PHP
$number = 10;
echo "The value of the variable is: " . $number;
```

In this code, we create a variable called `$number` and assign it a value of `10`. Then, using the `echo` statement, we print out a message along with the value of the variable. The output of this code will be:

```
The value of the variable is: 10
```

This can be helpful in understanding the flow of the code and identifying which variables are holding which values.

You can also print out the contents of an array or object using the `print_r()` function:

```PHP
$fruits = array("apple", "banana", "orange");
print_r($fruits);
```

The output will be:

```
Array
(
    [0] => apple
    [1] => banana
    [2] => orange
)
```

This allows you to see the structure and contents of complex data types, which can be useful when debugging.

## Deep Dive

In addition to the basic `echo` and `print` statements, there are various ways to print debug output in PHP. One method is by using the `var_dump()` function, which displays the data type, value, and length of a variable. This can be especially helpful for troubleshooting issues related to data types.

Another useful function is `debug_print_backtrace()`, which prints a backtrace of the function calls that lead to the current point in the code. This can help in understanding the flow of the program and identifying where the issue may have originated.

It's important to note that while print-debugging can be helpful, it should not be used as a permanent solution. It's best to remove any print statements once the issue has been identified and resolved to ensure clean and efficient code.

## See Also

- [PHP Manual: Debugging](https://www.php.net/manual/en/book.debug.php)
- [Debugging in PHP: A Beginner's Guide](https://www.cloudways.com/blog/php-debugging/)
- [5 Tips for Effective Debugging in PHP](https://www.telerik.com/blogs/5-tips-for-effective-debugging-in-php)