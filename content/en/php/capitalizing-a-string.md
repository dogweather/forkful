---
title:                "PHP recipe: Capitalizing a string"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String?

Capitalizing a string is a common task in programming, especially in web development. It involves converting the first letter of a word or sentence into uppercase, while the rest of the letters remain lowercase. This can be useful when displaying data or for input validation purposes.

## How To Do It

To capitalize a string in PHP, we can use the built-in function `ucfirst()`. This function takes a string as input and returns the same string with the first letter capitalized. For example:

```PHP
$string = "programming is fun";
echo ucfirst($string);
```

The output of this code would be:

```
Programming is fun
```

We can also use the `ucwords()` function to capitalize the first letter of each word in a string. For example:

```PHP
$string = "web development is important";
echo ucwords($string);
```

The output of this code would be:

```
Web Development Is Important
```

Both of these functions are useful for formatting data before displaying it on a web page or for validating user input.

## Deep Dive into Capitalization

In PHP, we can also use the `mb_convert_case()` function to handle multi-byte characters, such as accented letters, when capitalizing strings. This function takes a third argument that specifies the encoding to be used. For example:

```PHP
$string = "résumé";
echo mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
```

The output of this code would be:

```
Résumé
```

It's important to note that the `ucfirst()` and `ucwords()` functions will only capitalize the first letter of the string, regardless of the number of words or accents. Whereas the `mb_convert_case()` function will take into account the special characters and capitalizes them correctly.

## See Also

- [PHP ucfirst() function](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP ucwords() function](https://www.php.net/manual/en/function.ucwords.php)
- [PHP mb_convert_case() function](https://www.php.net/manual/en/function.mb-convert-case.php)