---
title:                "Capitalizing a string"
html_title:           "PHP recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing strings is a common task in programming, especially when dealing with user-provided data or when formatting output. It ensures that text is presented in a consistent and readable manner, making it easier for users to understand.

## How To
To capitalize a string in PHP, we can use the `ucwords()` and `strtoupper()` functions. These functions perform different types of capitalization, as shown in the examples below:

```PHP
$string = "hello world";

//using ucwords() to capitalize first letter of each word
echo ucwords($string); //output: Hello World

//using strtoupper() to capitalize all letters
echo strtoupper($string); //output: HELLO WORLD
```

It's important to note that the original string remains unchanged, as these functions return a new string with the desired capitalization. To save the changes, we need to assign the function's output to a variable or directly overwrite the original string.

We can also use the `mb_convert_case()` function to capitalize strings that contain multibyte characters, such as letters with accents or diacritics.

```PHP
$string = "jàmestown";
echo mb_convert_case($string, MB_CASE_TITLE, "UTF-8"); //output: Jàmestown
```

## Deep Dive
PHP offers a variety of functions for different capitalization needs. The `ucfirst()` function capitalizes only the first letter of a string, while `lcfirst()` lowercase the first letter. These functions are useful when we only want to modify the first letter of a string without affecting the rest.

Another handy function is `ucwords()`, which capitalizes the first letter of each word in a string. It's helpful when formatting names, titles, or sentences. However, it's worth noting that this function may produce unexpected results with certain characters, such as hyphens or quotes.

PHP also has the `mb_strtoupper()` and `mb_strtolower()` functions for manipulating strings with multibyte characters. These functions behave similarly to their single-byte counterparts, but they can handle a wider range of characters.

Lastly, we can also utilize regular expressions to capitalize strings in PHP. This approach gives us more flexibility to handle special cases or to create custom capitalization patterns.

See Also 
- PHP String Functions: https://www.php.net/manual/en/ref.strings.php
- Regular Expressions in PHP: https://www.php.net/manual/en/book.pcre.php