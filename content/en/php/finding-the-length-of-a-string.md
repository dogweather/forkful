---
title:                "Finding the length of a string"
html_title:           "PHP recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

There are a few scenarios where you might need to find the length of a string in PHP. For example, if you are building a password strength checker, you would need to find the length of the user's password to determine how secure it is. Additionally, if you are implementing character limits in a form field, you would also need to find the length of the input string to validate it.

## How To

Finding the length of a string in PHP is quite simple. All you need to do is use the built-in function `strlen()`. Let's take a look at an example:

```PHP
<?php 
$string = "Hello World!"; 
echo strlen($string); //outputs 12 
?>
```

In this example, we have declared a string variable with the value of "Hello World!" and used the `strlen()` function to find its length. The output will be 12 because there are 12 characters in the string, including spaces.

If you want to find the length of a user input, you can combine the `strlen()` function with the `$_POST` or `$_GET` variables, depending on how the form was submitted. Here's an example:

```PHP
<?php 
$input = $_POST['input_field']; 
echo strlen($input); 
?> 
```

## Deep Dive

Behind the scenes, the `strlen()` function in PHP calculates the number of bytes in a string, not the actual number of characters. This is important to keep in mind if you are working with multibyte characters or special characters, as they can take up more than one byte.

However, PHP also has a `mb_strlen()` function that counts the number of characters instead of bytes. This is particularly useful if you are working with languages that use multibyte characters, such as Chinese or Arabic.

Another important thing to note is that the `strlen()` function is case-sensitive. For example, if you have a string with both uppercase and lowercase letters, the function will return different lengths based on the case. If you want to get a case-insensitive length, you can use the `mb_strlen()` function with the `strtolower()` function, which converts all characters to lowercase before counting them.

## See Also

- PHP `strlen()` documentation: https://www.php.net/manual/en/function.strlen.php
- PHP `mb_strlen()` documentation: https://www.php.net/manual/en/function.mb-strlen.php
- PHP `strtolower()` documentation: https://www.php.net/manual/en/function.strtolower.php