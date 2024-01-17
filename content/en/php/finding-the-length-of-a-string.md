---
title:                "Finding the length of a string"
html_title:           "PHP recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is a common task in programming where the number of characters in a given string is calculated. This is useful for tasks such as validating input, manipulating text, and creating dynamic content. 

## How to:

To find the length of a string in PHP, we can use the `strlen()` function. This function takes in a string as its parameter and returns the number of characters in that string.

```PHP
// Example string
$string = "Hello World";

// Using strlen() function
$length = strlen($string);

echo $length;
// Output: 11
```

In the above example, the string "Hello World" is assigned to the variable `$string` and then the `strlen()` function is used to find its length. The output is then displayed using the `echo` statement.

## Deep Dive

Historically, finding the length of a string was a more complex task as it required counting each character in the string. However, with the advancement of programming languages, built-in functions like `strlen()` have made this task much simpler. 

In PHP, there are alternative ways of finding the length of a string such as using the `mb_strlen()` function for multibyte strings or the `count()` function for arrays. However, the `strlen()` function is the most commonly used for basic strings.

Implementation-wise, the `strlen()` function uses a loop to count the characters in a string. This loop continues until it reaches the end of the string, making it an efficient method for finding the length of a string even for longer strings.

## See Also

To learn more about finding the length of a string in PHP, check out the official PHP documentation on the `strlen()` function: https://www.php.net/manual/en/function.strlen.php

For alternative methods of finding the length of a string, explore the `mb_strlen()` function: https://www.php.net/manual/en/function.mb-strlen.php

And for finding the length of arrays, check out the `count()` function: https://www.php.net/manual/en/function.count.php