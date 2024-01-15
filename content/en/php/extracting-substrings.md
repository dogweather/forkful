---
title:                "Extracting substrings"
html_title:           "PHP recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to grab a smaller portion of a larger string? This could be for parsing data, formatting text, or any other reason. In PHP, we can easily accomplish this by extracting substrings.

## How To

Extracting substrings in PHP is a simple task that can be achieved using the built-in `substr()` function. Let's take a look at some code examples to see how it works:

```
<?php
// String we want to extract from
$string = "Hello World";

// Extracting from start index 0, with a length of 5 characters
$substring = substr($string, 0, 5);

// Output: Hello
echo $substring;

// Extracting from start index 6 until the end of the string
$substring = substr($string, 6);

// Output: World
echo $substring;
?>
```

As shown in the above examples, `substr()` takes in three parameters - the string we want to extract from, the starting index, and the optional length of characters to extract. If no length is specified, it will extract everything from the starting index until the end of the string.

We can also use negative indices to count backwards from the end of the string. Let's see this in action:

```
<?php
$string = "Lorem ipsum dolor sit amet";

// Extracting from start index -16, with a length of 5 characters
$substring = substr($string, -16, 5);

// Output: dolor
echo $substring;

// Extracting from start index -4 until the end of the string
$substring = substr($string, -4);

// Output: amet
echo $substring;
?>
```

In addition, we can use `substr()` to replace a portion of the string with another string, using the optional fourth parameter. This can be useful for formatting or manipulating text. Here's an example:

```
<?php
$string = "apple banana grape";

// Replacing "banana" with "orange"
$new_string = substr_replace($string, "orange", 6, 6);

// Output: apple orange grape
echo $new_string;
?>
```

## Deep Dive

The `substr()` function is a part of the core set of string functions in PHP. It primarily works with bytes and has some limitations when dealing with multibyte characters, as it counts each byte as one character. This can lead to unexpected results when working with non-English languages.

To avoid this issue, we can use the `mb_substr()` function, which is a multibyte version of `substr()`. It takes the same parameters and produces the same output, but properly handles multibyte characters by counting them as individual characters. Here's an example:

```
<?php
$string = "こんにちは世界";

// Extracting from start index 3, with a length of 3 characters
$substring = mb_substr($string, 3, 3);

// Output: 界
echo $substring;
?>
```

## See Also

- [PHP: substr() function](https://www.php.net/manual/en/function.substr.php)
- [PHP: mb_substr() function](https://www.php.net/manual/en/function.mb-substr.php)