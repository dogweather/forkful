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

## What & Why?

Printing debug output refers to the act of displaying the values of variables and other information during the execution of a program. This is a common practice among programmers as it allows them to track the behavior of their code and identify errors or potential issues.

## How to:

To print debug output in PHP, you can use the ```print_r()``` or ```var_dump()``` functions. The ```print_r()``` function allows you to view the contents of a variable, while ```var_dump()``` provides a more detailed and structured output.

Example code:
```PHP
$fruit = "apple";
print_r($fruit); // displays: apple
var_dump($fruit); // displays: string(5) "apple"
```

## Deep Dive:

1. Historical Context:
Debugging has been a fundamental part of programming since its inception. In the early days, programmers would use printing debug output as a way to check the state of their programs, as more advanced debugging tools were not available.

2. Alternatives:
Besides printing debug output, there are other ways to debug code, such as using a debugger tool or logging errors to a file. However, many programmers still prefer the simplicity and effectiveness of printing output.

3. Implementation Details:
The ```print_r()``` and ```var_dump()``` functions are built-in to PHP, making them easily accessible for developers. They also have some useful parameters that can be used to customize the output, such as ```print_r($fruit, true)``` to return the output as a string instead of displaying it.

## See Also:

- [Debugging in PHP](https://www.php.net/manual/en/debugger.php)
- [PHP Debugging with Xdebug](https://www.jetbrains.com/help/phpstorm/debugging-with-xdebug.html)
- [PHP Error Logging](https://www.php.net/manual/en/function.error-log.php)