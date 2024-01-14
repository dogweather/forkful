---
title:    "PHP recipe: Converting a string to lower case"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why
There are many reasons why a programmer may need to convert a string to lower case. One common reason is for input validation, to ensure that user input matches a particular format or is in a specific case. Lower case conversion can also be useful for sorting data and avoiding duplicate entries.

## How To
Converting a string to lower case in PHP is a simple task that can be accomplished using the `strtolower()` function. Let's take a look at some examples:

```
<?php
// Basic example
$string = "HELLO WORLD";
echo strtolower($string); // output: "hello world"

// Example with user input
$user_input = $_POST["name"];
echo strtolower($user_input); // if user enters "John", output: "john"
?>
```

In addition to `strtolower()`, there are other functions that can assist with string manipulation, such as `ucfirst()` which converts the first letter of a string to upper case, and `ucwords()` which converts the first letter of each word to upper case.

## Deep Dive
Converting a string to lower case may seem like a simple task, but there are a few things to keep in mind.

1. Multibyte characters: When dealing with strings that contain non-ASCII characters, it's important to use the `mb_strtolower()` function instead of `strtolower()`. This ensures that the conversion is done correctly for multibyte characters.

2. Custom conversions: While the `strtolower()` function follows the basic ASCII conversion rules, you may need to perform a custom conversion for specific languages or situations. In this case, you can use a combination of `mb_strtolower()` and `strtr()`, which allows for more flexibility in defining your own conversion rules.

## See Also
- [PHP string functions](https://www.php.net/manual/en/ref.strings.php)
- [PHP Multibyte String Functions](https://www.php.net/manual/en/ref.mbstring.php)
- [PHP strtr() function](https://www.php.net/manual/en/function.strtr.php)