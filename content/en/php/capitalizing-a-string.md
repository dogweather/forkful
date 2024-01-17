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

Capitalizing a string means converting the first letter of each word in a string to uppercase. Programmers do this for various reasons, such as creating a more visually appealing output or following proper capitalization rules for titles and names.

## How to:

To capitalize a string in PHP, you can use the built-in function `ucwords()`. It takes a string as an input and returns a new string with the first letter of each word capitalized.

Example:
```
$input = "hello world";
$output = ucwords($input);
echo $output;
```

Output:
```
Hello World
```

If you only want to capitalize the first letter of the first word, you can use the function `ucfirst()` instead.

Example:
```
$input = "hello world";
$output = ucfirst($input);
echo $output;
```

Output:
```
Hello world
```

## Deep Dive:

Capitalizing strings has its origins in print media, where it was used to make text stand out and appear more formal. In programming, it is often used in conjunction with string manipulation functions or for formatting data.

There are alternative ways to capitalize a string in PHP, such as using regular expressions or built-in string functions like `substr()` and `mb_strtoupper()`. However, the `ucwords()` and `ucfirst()` functions are the most straightforward and efficient methods.

It is worth noting that the `ucwords()` function follows the `setlocale()` setting, which is used to determine the appropriate case conversion for a particular language. So, if you need to capitalize strings in a different language, make sure to set the locale accordingly.

## See Also:

- [PHP Manual: ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [PHP Manual: ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP Manual: setlocale()](https://www.php.net/manual/en/function.setlocale.php)