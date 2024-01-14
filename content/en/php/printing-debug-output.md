---
title:                "PHP recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/printing-debug-output.md"
---

{{< edit_this_page >}}

##Why Print Debug Output?

Debugging is an essential part of any programming process. It allows developers to identify and fix errors in their code, leading to more efficient and reliable applications. One useful tool for debugging is printing debug output, which allows developers to see the values of specific variables and functions at different stages of the code execution. This can help pinpoint the source of a bug and aid in troubleshooting.

##How To Print Debug Output in PHP

To print debug output in PHP, we can use the `print_r()` function. This function takes in an argument and displays its value, including any nested arrays or objects.

```
<?php
//create a variable with a value
$name = "John";

//print out the value of the variable
print_r($name);

//output: John
?>
```

We can also use this function to print out the values of arrays and objects. 

```
<?php
//create an array with key-value pairs
$fruits = ["apple" => "red", "banana" => "yellow", "orange" => "orange"];

//print out the values of the array
print_r($fruits);

//output: Array ( [apple] => red [banana] => yellow [orange] => orange )
?>
```

Lastly, we can use `var_dump()` function to print out the data type and values of a variable. This can be helpful when debugging a complex code. 

```
<?php
//create a variable with a Boolean value
$language = true;

//print out the datatype and value
var_dump($language);

//output: bool(true)
?>
```

##Deep Dive into Printing Debug Output

While the `print_r()` and `var_dump()` functions are helpful for basic debugging, there are other ways to enhance your debugging process. For example, you can use the `error_log()` function to print out debug information to a log file instead of displaying it on the screen. This can be useful when working on a live server or when debugging long scripts.

Additionally, you can add conditional statements around your debug output code to control when it is displayed. This can prevent excessive output in large projects and make your debugging process more efficient.

##See Also

For more information on debugging in PHP, check out these helpful resources:

- PHP Manual: [Debugging in PHP](https://www.php.net/manual/en/debugger.php)
- SitePoint: [Debugging PHP with xdebug](https://www.sitepoint.com/debugging-php-xdebug/)
- PHP Academy: [Debugging PHP - Print and Error Log](https://www.youtube.com/watch?v=_X_vCncQaf0)

Happy debugging!