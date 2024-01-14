---
title:                "PHP recipe: Extracting substrings"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Are you new to PHP programming and wondering what extracting substrings means? Or are you just looking to improve your skills? Whatever the case may be, extracting substrings is an essential skill to have in your programming toolkit. Substrings are portions of a larger string, and being able to extract them allows you to manipulate data and perform various tasks effectively. So, let's dive into how you can do it!

## How To 
Extracting substrings is a straightforward process in PHP. The `substr()` function is used to extract a specific portion of a string based on the starting point and length of the substring. Let's take a look at an example:

```PHP
$string = "Hello World!";
$substr = substr($string, 0, 5);

echo $substr; // Outputs "Hello"
```

In this example, we have a string "Hello World!", and we use the `substr()` function to extract the first five characters, starting from the index 0. The output of this code would be "Hello". 

You can also extract substrings from the end of a string by using a negative starting point. For example:

```PHP
$string = "Hello World!";
$substr = substr($string, -6);

echo $substr; // Outputs "World!"
```

In this example, we extract the last six characters from the string "Hello World!" starting from the end. The output will be "World!".

You can also use the `strpos()` function to find the position of a specific character or substring within a string and then use that position in the `substr()` function to extract a substring. For example:

```PHP
$string = "Hello World!";
$position = strpos($string, "W");

$substr = substr($string, $position);

echo $substr; // Outputs "World!"
```
In this example, we find the position of the letter "W" within the string "Hello World!" and use it to extract the substring starting from that position. The output will be "World!".

You can also use the `str_replace()` function to replace specific substrings within a string with another substring. For example:

```PHP
$string = "Hello World!";
$modified = str_replace("World", "Universe", $string);

echo $modified; // Outputs "Hello Universe!"
```

In this example, we use the `str_replace()` function to replace the substring "World" with "Universe" within the string "Hello World!". The output will be "Hello Universe!".

## Deep Dive

Now that you have an understanding of how to extract substrings, here are some additional things to keep in mind:

- The `substr()` function is case-sensitive, so remember to consider the case when extracting a substring.
- If the length parameter is not specified, the `substr()` function will extract the substring starting from the given position until the end of the string.
- You can also use a variable instead of hardcoded values for the starting position and length of the substring.
- Take advantage of other string functions, like `strlen()` to determine the length of a string, and `strrev()` to reverse a string, when extracting substrings.

See Also
- Official PHP Manual for [substr()](https://www.php.net/manual/en/function.substr.php), [strpos()](https://www.php.net/manual/en/function.strpos.php) and [str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [String Functions in PHP](https://www.w3schools.com/php/php_ref_string.asp) from W3Schools
- [Mastering PHP substr() Function with Examples](https://www.codeofaninja.com/2016/04/mastering-php-substr-function-with-examples.html) by CodeofaNinja