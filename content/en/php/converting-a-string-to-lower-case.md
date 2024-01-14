---
title:    "PHP recipe: Converting a string to lower case"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lowercase is a common task in programming, especially when dealing with user input. This ensures that the input is consistent and can be easily compared to other strings without worrying about casing differences. In this blog post, we will explore different ways to convert strings to lowercase in PHP.

## How To

### Using strtolower() function

The simplest way to convert a string to lowercase in PHP is by using the `strtolower()` function. Here's an example of how to use it:

```PHP
<?php
$string = "Hello World";
echo strtolower($string);
```

The output of this code will be: "hello world". As you can see, the function converts all the characters in the string to lowercase.

### Using mb_strtolower() function

If you are working with multibyte strings (such as non-English characters), the `mb_strtolower()` function comes in handy. It has the same functionality as `strtolower()` but supports multibyte characters. Here's an example:

```PHP
<?php
$string = "Привет мир";
echo mb_strtolower($string);
```

The output will be: "привет мир".

### Using the strCASECASE() function

The `strcasecase()` function is another option for converting strings to lowercase. It converts the first character of each word to lowercase, leaving the rest of the characters as is. Here's an example:

```PHP
<?php
$string = "I LIKE TO CODE";
echo strCASECASE($string);
```

The output will be: "i LIKE tO cODE".

## Deep Dive

PHP provides several functions for string manipulation, and understanding how these functions work can help you choose the best option for your specific needs. When using `strtolower()` and `mb_strtolower()`, it's important to note that they both return a lowercase version of the string, leaving the original string unchanged. On the other hand, `strcasecase()` modifies the original string in place.

Furthermore, when dealing with non-English characters, make sure to set the correct character encoding for your string using `mb_internal_encoding()` to avoid any unexpected results.

## See Also

- [String functions in PHP](https://www.php.net/manual/en/ref.strings.php)
- [Multibyte string functions in PHP](https://www.php.net/manual/en/ref.mbstring.php)
- [PHP string manipulation: A beginner's guide](https://www.elegantthemes.com/blog/wordpress/php-string-manipulation)