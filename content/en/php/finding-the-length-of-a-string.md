---
title:                "PHP recipe: Finding the length of a string"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with strings in PHP, it is often necessary to determine the length of a string. This can be useful for various reasons, such as validating user input or manipulating the string in a specific way. In this blog post, we will explore how to find the length of a string in PHP.

## How To

Finding the length of a string in PHP is a simple task and can be achieved in a few different ways.

First, we can use the built-in `strlen()` function. This function takes a string as an argument and returns the length of the string as an integer. For example:

```PHP
$str = "Hello world!";
echo strlen($str); // Outputs: 12
```

Another way to find the length of a string is by using the `mb_strlen()` function. This function is similar to `strlen()`, but it takes into account multibyte characters. This is especially important if you are working with different languages that use multibyte characters, such as Chinese or Japanese. Here's an example:

```PHP
$str = "こんにちは、世界！";
echo mb_strlen($str); // Outputs: 7
```

Lastly, we can also use the `str_split()` function to split a string into an array, and then use the `count()` function to determine the length of the array. This may be useful if you need to manipulate the string further after finding its length. Here's an example:

```PHP
$str = "Lorem ipsum dolor sit amet";
$str_arr = str_split($str);

echo count($str_arr); // Outputs: 26
```

## Deep Dive

It's important to remember that the length of a string is determined by the number of characters, not words. This means that spaces and punctuation marks are also counted as characters.

Additionally, the `strlen()` and `mb_strlen()` functions count the bytes in a string, which can affect the length if you have special characters or symbols in the string. In this case, it may be useful to use the `mb_strlen()` function to accurately determine the length.

## See Also

- [PHP Manual: String Functions](https://www.php.net/manual/en/ref.strings.php)
- [w3schools: PHP Strings](https://www.w3schools.com/php/php_string.asp)
- [GeeksforGeeks: Find length of a string in PHP](https://www.geeksforgeeks.org/how-to-find-the-length-of-a-string-in-php/)