---
title:    "PHP recipe: Extracting substrings"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Why Extracting Substrings Matters

When working with large amounts of text data, it is often necessary to extract specific portions or substrings from strings. This can be useful for tasks such as data cleaning, data analysis, or data manipulation. In PHP, there are several built-in functions that make extracting substrings easy and efficient.

# How To Extract Substrings in PHP

To extract a substring in PHP, we can use the `substr()` function. This function takes in three parameters: the string to extract from, the starting index, and the length of the substring to extract.

```
<?php
$string = "Hello World";
$substring = substr($string, 0, 5);
echo $substring;
// Output: Hello
?>
```

In the example above, we extracted the first 5 characters from the string "Hello World".

We can also use negative numbers as the starting index to extract substrings from the end of the string. For example, `substr($string, -5)` would extract the last 5 characters from the string.

Another useful function for extracting substrings is `mb_substr()`. This function is similar to `substr()` but takes into account multibyte characters, making it more reliable for handling different languages.

```
<?php
$string = "안녕하세요";
$substring = mb_substr($string, 0, 2, "UTF-8");
echo $substring;
// Output: 안녕
?>
```

# Deep Dive into Substring Extraction

In addition to the built-in functions, PHP also has a powerful regular expression library for manipulating strings. This can be especially useful for extracting substrings based on specific patterns.

For example, we can use the `preg_match()` function to extract all numbers from a string using regular expressions.

```
<?php
$string = "My phone number is 555-123-4567";
$output = preg_match("/[0-9]+/", $string, $matches);
echo $matches[0];
// Output: 5551234567
?>
```

Regular expressions allow for complex and precise substring extraction, making them a valuable tool in a developer's toolkit.

# See Also

- [PHP Manual on `substr()`](https://www.php.net/manual/en/function.substr.php)
- [PHP Manual on `mb_substr()`](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP Manual on `preg_match()`](https://www.php.net/manual/en/function.preg-match.php)