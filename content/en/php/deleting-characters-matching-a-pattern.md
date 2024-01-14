---
title:                "PHP recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Have you ever encountered a situation where you needed to remove certain characters from a string in your PHP code? Perhaps it was a pesky line break or a special character causing unwanted formatting. Whatever the case, deleting characters matching a certain pattern can be a useful tool in your PHP programming arsenal.

## How To
To delete characters matching a pattern in PHP, we can use the `preg_replace()` function. Let's take a look at an example:

```
<?php 
$string = "Hello, [World]! How are you?";
$new_string = preg_replace("/\[.*?\]/", "", $string);
echo $new_string;

// Output: Hello, ! How are you?
?>
```

In this example, we use the `preg_replace()` function to remove any characters between square brackets, including the brackets themselves. Let's break down the parameters of this function:
- The first parameter is the regular expression pattern we want to match. In this case, we use the opening bracket, followed by any number of characters, followed by the closing bracket.
- The second parameter is the replacement string, which we leave empty since we want to delete the matched characters.
- The third parameter is the original string that we want to perform this operation on.

The `preg_replace()` function returns a new string with the matched characters removed, but it does not modify the original string. That's why we assign the result to a new variable `$new_string` and then echo it out.

## Deep Dive
The `preg_replace()` function uses regular expressions (regex) to find and replace text in a string. Regular expressions are a powerful pattern matching tool that allows us to specify patterns of characters we want to search for in a string.

In our example, we used the `.*?` pattern to match any number of characters, including spaces and special characters, between the brackets. The `?` modifier makes this pattern non-greedy, meaning it will stop matching as soon as it finds the first closing bracket.

If you want to learn more about regular expressions and how to use them in PHP, [this tutorial](https://www.php.net/manual/en/function.preg-replace.php#example-826) from the official PHP documentation is a great place to start.

## See Also
- [PHP documentation on preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions 101: A Beginner's Guide](https://www.regular-expressions.info/tutorial.html)
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)