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

## What & Why?

Capitalizing a string means changing the first letter of each word in the string to upper-case. Programmers do this to improve legibility and readability of the text, or for standardizing text input.

## How to:

Here's a short and sweet code example that capitalizes a string in PHP.

```PHP
<?php
$string = 'say hello to PHP';
$capString = ucwords($string);
echo $capString;
?>
```
The `ucwords()` function in PHP changes the first letter of each word in the string to uppercase. The output you'll see is:

```
Say Hello To PHP
```

Now let's see an example that only capitalizes the first character of the string:

```PHP
<?php
$string = 'say hello to PHP';
$capString = ucfirst($string);
echo $capString;
?>
```
The `ucfirst()` function capitalizes the first letter of the string. The output you'll get is:

```
Say hello to PHP
```

## Deep Dive 

PHP's `ucwords()` and `ucfirst()` functions were introduced back in PHP 4, so they are well-established in the language. 

An alternative to `ucwords()` for capitalizing every word would be using the `mb_convert_case()` function with the `MB_CASE_TITLE` parameter. This is particularly useful when dealing with multibyte encodings, as `ucwords()` doesn't play well with non-ASCII characters. 

```PHP
<?php
$string = 'say hello to PHP';
$capString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capString;
?>
```
This will output:

```
Say Hello To PHP
```

These PHP string functions are implemented at the C level, using ASCII logic for capitalization. That's why they're super speedy and reliable.

## See Also

For more in-depth knowledge about these functions and string manipulation in PHP, check out the following resources:

- [PHP ucwords() function](https://www.php.net/manual/en/function.ucwords.php)
- [PHP ucfirst() function](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP mb_convert_case() function](https://www.php.net/manual/en/function.mb-convert-case.php)
- [PHP Manual: String Functions](https://www.php.net/manual/en/ref.strings.php)