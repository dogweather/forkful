---
title:                "शब्द की लंबाई पता करना"
html_title:           "PHP: शब्द की लंबाई पता करना"
simple_title:         "शब्द की लंबाई पता करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means determining the number of characters in that particular string. In simple terms, it means counting the number of letters, numbers, spaces, and symbols present in a text. Programmers need to find the length of a string for various reasons, such as data validation, calculations, and manipulating data in a specific format.

## How To:
To find the length of a string in PHP, you can use the `strlen()` function. This function takes a string as an argument and returns the number of characters in that string. Let's take a look at an example:

```PHP
<?php
    $name = "John Doe";
    $length = strlen($name); // $length will be 8
    echo "The length of the string is $length.";
?>
```

In this example, we have assigned a string containing the name "John Doe" to a variable called `$name`. Then we use the `strlen()` function to find the length of this string and store the result in a variable called `$length`. Finally, we use the `echo` statement to print the result, which in this case is the length of the string.

You can also use the `mb_strlen()` function to find the length of a string in PHP, especially if your string contains multi-byte characters, such as special symbols in Hindi. This function takes an additional parameter for specifying the character encoding. Here's an example:

```PHP
<?php
    $text = "यह हिंदी में एक शब्द है";
    $length = mb_strlen($text, 'utf-8'); // $length will be 15
    echo "The length of the string is $length.";
?>
```

In this example, we have used the `mb_strlen()` function to find the length of a string in Hindi, which is 15 characters long. We have also specified the character encoding as UTF-8, which is the standard for Hindi characters.

## Deep Dive:
The concept of finding the length of a string is not exclusive to PHP. It is a common task in programming languages, and most languages have built-in functions for this purpose. However, in certain cases, such as in C or Assembly language, the programmer may have to write their own code to calculate the length of a string.

In PHP, the `strlen()` function returns the number of bytes in a string, while the `mb_strlen()` function returns the number of characters. This difference is important to keep in mind, especially when dealing with multi-byte characters in different languages.

Alternatively, you can also use the `count()` function to find the length of a string in PHP. This function counts the number of elements in an array, and since PHP treats strings as arrays of characters, you can use this function to find the length of a string as well.

## See Also:
To learn more about finding the length of a string in PHP, you can refer to the official PHP documentation on the `strlen()` function: https://www.php.net/manual/en/function.strlen.php

For more information on character encoding and multi-byte support in PHP, you can check out the PHP documentation on mbstring: https://www.php.net/manual/en/book.mbstring.php

If you are interested in learning more about strings and their manipulation in PHP, here's a comprehensive tutorial: https://www.geeksforgeeks.org/php-strings/

Happy coding! 🚀