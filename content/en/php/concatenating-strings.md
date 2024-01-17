---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in PHP refers to the process of joining multiple strings together into a single string. Programmers use this technique to combine different pieces of text, variables, or data into one readable output. It is a commonly used function in PHP and is essential for creating dynamic and versatile web applications.

## How to:

Concatenating strings in PHP is simple and can be done in multiple ways. One of the most common ways is by using the concatenation operator (.) to join two strings together. Let's take a look at an example:

```
<?php
$name = "John";
$age = 25;
echo $name . " is " . $age . " years old.";
```

The output of this code will be: "John is 25 years old." In this example, we use the concatenation operator to join the strings "John", " is ", and " years old." together, with the value of the variable $age in between.

Another way to concatenate strings is by using the PHP `implode()` function. This function takes an array of strings and combines them into one string using a specified separator. Here's an example:

```
<?php
$fruits = array("apple", "orange", "banana");
echo implode(", ", $fruits);
```

The output of this code will be: "apple, orange, banana." In this example, we use the `implode()` function to join the strings "apple", "orange", and "banana" together, with the separator ", " in between each string.

## Deep Dive:

Concatenating strings has been a fundamental feature of programming languages for a long time. In PHP, it is not only limited to strings but can also be used for merging arrays and objects.

There are also alternative ways to concatenate strings in PHP, such as the `sprintf()` function, which allows for more control over the formatting of the output. It is also worth noting that using the concatenation operator is faster and more efficient than using the `sprintf()` function.

Internally, string concatenation in PHP is implemented using a technique called temporary variables. This means that when strings are concatenated, a temporary variable is created to hold the new string, and then the original variable is replaced with the new one. This allows for more efficient memory management and better performance in large-scale applications.

## See Also:

To learn more about concatenating strings in PHP, here are some helpful resources:

- [PHP Manual on string operators](https://www.php.net/manual/en/language.operators.string.php)
- [W3Schools Concatenation Tutorial](https://www.w3schools.com/php/php_string_concatenation.asp)
- [PHP Video Tutorial for Beginners](https://www.youtube.com/watch?v=2eebptXfEv0)