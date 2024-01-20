---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings simply refers to the selection of a part or parts from a string. This is done by programmers to ensure data is displayed/manipulated in usable and meaningful chunks.

## How to:

We can use the `substr()` function in PHP to extract substrings. Two key parameters are required: the starting point and length of the substring.

```PHP
<?php
$string = "Welcome to PHP programming!";

// Extract "Welcome" from string
$substring = substr($string, 0, 7);
echo $substring;  // Displays: Welcome
```

If you need to extract from the end of the string, use a negative starting point:

```PHP
// Extract "programming!" from string
$substring = substr($string, -12);
echo $substring;  // Displays: programming!
```

## Deep Dive

The `substr()` function, a mainstay since PHP 4, has remained relevant because of its simple, yet powerful approach to string manipulation. 

An alternative to `substr()` is `mb_substr()`. This function works similar but its use is recommended when dealing with multi-byte characters like Japanese or emojis. 

While `substr()` operates on bytes, `mb_substr()` works with characters, ideal for internationalization (i18n).

Execution wise, the `substr()` function has a slight performance advantage, but in real-world scenarios, the difference is often negligible. 

## See Also

For further reading and resources regarding string manipulation, refer to the PHP documentation:

1. [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
2. [PHP: mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
3. [PHP: Strings - Manual](https://www.php.net/manual/en/language.types.string.php)

Be sure to understand the options, features and limitations of each function to pick the best tool for your task. Happy coding!