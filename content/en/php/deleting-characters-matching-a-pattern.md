---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deleting Characters Matching a Pattern in PHP

## What & Why?
In PHP, deleting characters matching a pattern involves removing all instances of certain letters, numbers, or special characters from a string. This is often crucial in data cleansing tasks, where it helps maintain data integrity and prevents errors in later operations.

## How to:
You can delete characters matching a pattern in PHP using the built-in `preg_replace` function. The function searches the string for a specified pattern and replaces it with another. So, to delete, we replace with an empty string.

Here's a small piece of code that deletes all numbers from a string:

```PHP
<?php
$string = "Hello123World456";
$result = preg_replace("/[0-9]/", "", $string);
echo $result; // Outputs: HelloWorld
?>
```
The pattern `"/[0-9]/"` matches any number between 0 and 9. 

## Deep Dive
The `preg_replace` function leverages Regular Expressions (RegEx) - a concept that dates back to the 1950s. The theory behind Regular Expressions was formulated by mathematician Stephen Kleene.

You could also use PHP's `str_replace` function to delete specific characters, but `preg_replace` provides more flexibility by supporting patterns.

Note that using `preg_replace` results in a new string with the characters removed, it does not alter the original string. If you want to update the original string, you would need to assign the result back to the original string variable.

## See Also:
To deepen your understanding of Regular Expressions and the `preg_replace` function, refer to the following resources:

- PHP Manual for `preg_replace`: [https://www.php.net/manual/en/function.preg-replace.php](https://www.php.net/manual/en/function.preg-replace.php)
- Regular Expressions Info: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Working with Strings in PHP: [https://www.php.net/manual/en/book.strings.php](https://www.php.net/manual/en/book.strings.php)