---
title:                "Converting a string to lower case"
html_title:           "PHP recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in any web development project. It helps ensure consistency and avoids errors when comparing strings or performing searches. Plus, it's a good practice for user input to always be converted to lower case before processing.

## How To

To convert a string to lower case in PHP, you can use the built-in function `strtolower()`. This function takes a string as an argument and returns a new string with all characters converted to lower case.

```PHP
$string = "Hello World!";
echo strtolower($string); // outputs "hello world!"
```

You can also use the `mb_strtolower()` function for multi-byte encodings, which is useful when dealing with languages such as Chinese or Japanese.

```PHP
$string = "こんにちは世界！";
echo mb_strtolower($string); // outputs "こんにちは世界！"
```

It's important to note that these functions only work for ASCII characters. For non-ASCII characters, you may need to use a different method depending on your encoding.

## Deep Dive

In PHP, strings are immutable, meaning they cannot be changed once they are created. So when converting a string to lower case, a new string is actually created with the lower case version of the original string. This is why we need to assign the result to a variable or use it directly in our code.

Additionally, when dealing with multi-byte encodings, it's important to make sure that both the string and the server encoding are set correctly. Otherwise, your conversion may not produce the desired results.

## See Also

Here are some additional resources for working with strings in PHP:

- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [Understanding Case Sensitivity in PHP](https://www.fullstackacademy.com/blog/case-sensitivity-in-php)
- [UTF-8 and PHP: Handling Collation and Normalization](https://www.toptal.com/php/a-utf-8-primer-for-php-and-mysql)