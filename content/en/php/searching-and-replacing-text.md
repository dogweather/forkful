---
title:                "Searching and replacing text"
html_title:           "PHP recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming where specific strings of characters are identified and replaced with other strings. This can be done for a variety of reasons such as correcting mistakes, updating code, or making changes to a large amount of text at once. Programmers use this technique to save time and ensure accuracy in their code.

## How to:

To search and replace text in PHP, use the built-in function `str_replace()`. Here's an example of how it works:

```PHP
// Define a string
$string = "Hello world!";

// Replace 'world' with 'there'
$new_string = str_replace('world', 'there', $string);

// Output the new string
echo $new_string;
```

This will output `Hello there!`. Another useful function for searching and replacing text is `str_ireplace()` which is case-insensitive. Here's an example:

```PHP
// Define a string
$string = "This is a Test.";

// Replace 'test' with 'example'
$new_string = str_ireplace('test', 'example', $string);

// Output the new string
echo $new_string;
```

This will output `This is a Example.`. You can also use `str_replace()` to replace multiple instances of a string at once. Here's an example:

```PHP
// Define a string
$string = "The red pen is on the table.";

// Replace 'red' and 'table' with 'blue' and 'desk'
$new_string = str_replace(['red', 'table'], ['blue', 'desk'], $string);

// Output the new string
echo $new_string;
```

This will output `The blue pen is on the desk.`.

## Deep Dive

PHP's `str_replace()` function was first introduced in PHP 4.0. The function takes in four parameters - the search string, the replacement string, the original string, and an optional fifth parameter for specifying the number of replacements to make. It uses a simple string replacement algorithm, making it an efficient method for replacing text in PHP.

Another alternative for searching and replacing text in PHP is using the `preg_replace()` function. This function uses regular expressions for pattern matching and can be more powerful than `str_replace()`. However, it is also more resource-intensive and should be used with caution, especially for large strings.

## See Also

Documentation for [PHP's str_replace() function](https://www.php.net/manual/en/function.str-replace.php).

Documentation for [PHP's preg_replace() function](https://www.php.net/manual/en/function.preg-replace.php).