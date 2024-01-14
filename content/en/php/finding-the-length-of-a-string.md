---
title:                "PHP recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

### Why
Have you ever needed to know the length of a string in your PHP code? Maybe you wanted to validate user input or shorten a string for display purposes. Regardless of the reason, knowing how to find the length of a string is a useful skill for any PHP programmer.

### How To
To find the length of a string in PHP, we can use the built-in function `strlen()`. This function takes a string as its parameter and returns the length of the string. For example:

```PHP
$string = "Hello World";
echo strlen($string);
```

The above code will output `11`, as there are 11 characters in the string "Hello World". This function works regardless of the content of the string, so it can also be used for non-English languages.

Another way to find the length of a string is to use the `mb_strlen()` function. This function is similar to `strlen()` but is specifically designed for multi-byte character sets (such as UTF-8). This is useful when dealing with strings that contain non-English characters. Here's an example:

```PHP
$string = "こんにちは世界";
echo mb_strlen($string);
```

The output for this code will be `5`, as the Japanese characters each count as one character in this function.

### Deep Dive
Internally, PHP represents strings as a sequence of bytes. This means that the length of a string is simply the count of those bytes, regardless of the actual characters they represent. This also means that the length of a string may differ depending on the character encoding being used.

The `strlen()` function uses the number of bytes to determine the length of a string, while `mb_strlen()` uses the actual characters. This is why `mb_strlen()` may give a different result for strings with non-English characters.

Additionally, the `strlen()` and `mb_strlen()` functions do not take into account any HTML tags that may be present in a string. To get the length of a string without HTML tags, we can use the `strip_tags()` function before using `strlen()` or `mb_strlen()`.

### See Also
- PHP Manual on `strlen()`: https://www.php.net/manual/en/function.strlen.php
- PHP Manual on `mb_strlen()`: https://www.php.net/manual/en/function.mb-strlen.php
- PHP Manual on `strip_tags()`: https://www.php.net/manual/en/function.strip-tags.php