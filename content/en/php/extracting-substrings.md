---
title:                "PHP recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extracting Substrings is Useful in PHP Programming

Substring extraction is a useful tool in PHP programming as it allows for the manipulation of specific portions of a string. This can be beneficial when working with large strings and needing to extract certain data or when trying to modify a portion of a string without affecting the rest.

## How To Extract Substrings in PHP

```PHP
// Example string
$string = "Welcome to my blog!";

// Using substr() to extract substring at index 11 with a length of 5
$substring = substr($string, 11, 5);
echo $substring; // Output: blog!
```

In the above example, we use the substr() function which takes in three parameters: the string we want to extract from, the starting index of the substring, and the length of the substring. By specifying a starting index and length, we can extract a specific portion of the original string.

```PHP
// Example string
$string = "Check out my website at www.example.com";

// Using strpos() and substr() to extract substring after "www."
$index = strpos($string, "www.") + 4; // Finds index of the start of "www."
$substring = substr($string, $index); // Extracts substring starting at index 12
echo $substring; // Output: www.example.com
```

In this example, we use a combination of the strpos() and substr() functions. With strpos(), we find the starting index of "www." and add 4 to account for the length of "www." itself. Then, we use substr() to extract the rest of the string starting from that index.

## Deep Dive: Understanding the Substr() Function

The substr() function is a powerful tool that can do more than just extract substrings. It can also be used to replace and insert portions of a string.

```PHP
// Example string
$string = "Hello, world!";

// Using substr() to replace "world" with "PHP"
$modified_string = substr_replace($string, "PHP", 7, 5);
echo $modified_string; // Output: Hello, PHP!
```

In this example, we use the substr_replace() function which takes in five parameters: the original string, the string we want to replace with, the starting index of the replacement, the length of the replacement, and an optional parameter for any inserted substring. Here, we replace "world" with "PHP" starting at index 7 and with a length of 5.

## See Also

- [PHP: substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP: substr_replace()](https://www.php.net/manual/en/function.substr-replace.php)
- [PHP: string functions](https://www.php.net/manual/en/ref.strings.php)