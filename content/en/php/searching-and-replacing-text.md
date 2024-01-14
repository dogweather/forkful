---
title:                "PHP recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, especially when working with large amounts of data. It can save time and effort by allowing you to make multiple changes at once, rather than manually editing each instance of the text.

## How To
To search and replace text in PHP, we can use the `str_replace` function. This function takes three parameters: the text to search for, the text to replace it with, and the string to search within. Let's take a look at an example:

```PHP
$text = "Hello World";
$new_text = str_replace("World", "Universe", $text);
echo $new_text;
```
Output: Hello Universe

In this example, we searched for the word "World" within the string "Hello World" and replaced it with "Universe". The `str_replace` function then returned the new string with the text replaced.

We can also use arrays as the first two parameters to make multiple replacements at once. Let's see how that works:

```PHP
$before = ["red", "green", "blue"];
$after = ["orange", "purple", "yellow"];
$text = "I love red, green, and blue.";
$new_text = str_replace($before, $after, $text);
echo $new_text;
```
Output: I love orange, purple, and yellow.

In addition to using `str_replace`, we can also use regular expressions to search and replace text in PHP. This gives us more flexibility in what we can search for and what we can replace it with. Here's an example using `preg_replace`:

```PHP
$text = "My favorite numbers are 123 and 456.";
$new_text = preg_replace("/[0-9]+/", "five", $text);
echo $new_text;
```
Output: My favorite numbers are five and five.

In this example, we used a regular expression to search for any sequence of numbers and replace it with the word "five". This is just one example of how regular expressions can be powerful tools for searching and replacing text.

## Deep Dive
Now that we have an understanding of how to search and replace text in PHP, let's take a deeper look at the different functions and methods available for this task.

First, we have the `str_replace` function, which we discussed earlier. This function is useful for simple, straightforward replacements. However, it is case-sensitive, so it may not always give us the desired results.

Next, we have the `str_ireplace` function, which works the same as `str_replace` but is case-insensitive. This can be helpful when we want to make replacements regardless of case.

For more advanced replacements, we can use the `preg_replace` function, which as we saw earlier, allows us to use regular expressions. This gives us more control over what we search for and what we replace it with.

Lastly, there is the `strtr` function, which stands for "string translate". This function allows us to make multiple replacements at once using arrays, similar to how we did it with `str_replace`, but it also preserves the order of the replacements. This can be useful if we need to make replacements in a specific order.

## See Also
- [PHP string functions](https://www.php.net/manual/en/ref.strings.php)
- [Regular expressions in PHP](https://www.php.net/manual/en/reference.pcre.pattern.php)
- [PHP.net - String functions](https://www.php.net/manual/en/ref.strings.php)