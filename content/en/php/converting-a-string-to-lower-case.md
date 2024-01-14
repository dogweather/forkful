---
title:                "PHP recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in PHP programming. It allows for better string comparison and manipulation, as well as conforming to certain coding standards. 

## How To

To convert a string to lower case in PHP, we can use the built-in function `strtolower()`. This function takes in a string and returns a new string with all characters converted to lower case. 

```
Example 1:
$string = "Hello World!";
echo strtolower($string);

Output:
hello world!
```

We can also convert a specific portion of a string to lower case by using the `substr()` function. This function allows us to specify the starting position and length of the portion we want to convert. 

```
Example 2:
$string = "PHP is a great language!";
echo substr_replace($string, strtolower(substr($string, 0, 3)), 0, 3);

Output:
php is a great language!
```

## Deep Dive

When converting a string to lower case, it is important to understand the different character encoding systems that may affect the output. The `strtolower()` function uses the current locale setting to determine how to convert characters. This means that the output may vary depending on the server's default system settings. 

In addition, it is important to note that the `strtolower()` function may not be effective for all languages or character sets. For example, in some languages such as Turkish, certain characters may not have a lower case equivalent. In these cases, it is recommended to use the `mb_strtolower()` function, which supports a wider range of character sets. 

## See Also

For more information on string manipulation in PHP, check out these resources:

- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [The mbstring extension in PHP](https://www.php.net/manual/en/mbstring.installation.php)