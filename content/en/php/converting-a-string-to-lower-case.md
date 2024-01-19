---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means turning all the uppercase characters of a string to lowercase. This process is a common programming task when comparing strings, since it provides consistency and mitigates problems caused by different text input.

## How to:

To convert a string to lower case in PHP, we use the `strtolower()` function. It's as easy as pie. Here's a quick example:

```PHP
<?php
$string = "HELLO, WORLD!";
echo strtolower($string);
?>
```
That will output:
```PHP
hello, world!
```
It's that simple!

## Deep Dive

The `strtolower()` function has been part of PHP since its infancy (PHP 4). This function works with all string formats and uses ASCII value comparison, thus making it universally applicable. 

If you're dealing with multibyte character strings (like Unicode), you may have to consider alternatives like `mb_strtolower()`, which supports a broader spectrum of characters.

In terms of implementation detail, `strtolower()` loops through the string, checks ASCII value of each character, and converts uppercase ASCII characters to their corresponding lowercase counterparts. This operation generally takes linear time in relation to the length of the string.

## See Also

1. `strtoupper()`: A counterpart to `strtolower()`, converts all characters to uppercase. [PHP strtoupper() Function - W3Schools](https://www.w3schools.com/php/func_string_strtoupper.asp)

2. `mb_strtolower()`: More suitable for multibyte string (like Unicode). [PHP: mb_strtolower - Manual](https://www.php.net/manual/en/function.mb-strtolower)

3. ASCII table: To understand ASCII value comparison, check this source. [ASCII Chart – RapidTables](https://www.rapidtables.com/code/text/ascii-table.html)

Feel free to deepen your understanding of PHP strings by exploring the above resources!