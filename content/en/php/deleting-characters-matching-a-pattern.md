---
title:                "Deleting characters matching a pattern"
html_title:           "PHP recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters that match a certain pattern is a common task in programming, as it allows for more efficient and accurate data manipulation. By removing specific characters from a given string or piece of data, we can narrow down our results and ultimately achieve our desired output.

## How to:
First, we need to define the pattern we want to match using the preg_replace() function. We do this by providing the pattern as the first argument, and the replacement string as the second argument. For example, if we want to remove all numbers from a given string, our pattern would be "/[0-9]/" and our replacement string would be an empty string.

Next, we need to specify the string or data that we want to apply the pattern to. This is done as the third argument in the preg_replace() function. For simplicity, let's use a string variable named $str.

Finally, we simply echo the result of the preg_replace() function to see the updated string without the specified characters. Here's an example code snippet:

```PHP
$str = "Hello, I am 28 years old!";
echo preg_replace("/[0-9]/", "", $str); // Output: Hello, I am years old!
```

## Deep Dive:
1. Historical Context: The preg_replace() function was first introduced in PHP 3, and has since become a widely used and reliable method for pattern matching and string replacements.
2. Alternatives: Another approach to deleting characters that match a pattern is using the str_replace() function, which replaces all occurrences of a string with another string. However, this may not be as efficient when dealing with complex patterns.
3. Implementation Details: The preg_replace() function uses regular expressions to define the pattern, which can include special characters for more specific matching. It also has additional optional arguments such as limit and count, which can be useful for fine-tuning the replacement process.

## See Also:
- [PHP preg_replace() function](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP str_replace() function](https://www.php.net/manual/en/function.str-replace.php)
- [Regular Expressions Tutorial](https://regexone.com/)
- [PHP Manual: Regular Expressions](https://www.php.net/manual/en/regexp.reference.php)