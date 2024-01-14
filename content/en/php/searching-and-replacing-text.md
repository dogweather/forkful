---
title:                "PHP recipe: Searching and replacing text"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming. It allows for efficient and accurate changes to be made to large amounts of text, without having to manually go through each line. In PHP, this can be done easily with the use of regular expressions.

## How To

To start, we will use the `preg_replace()` function in PHP to search and replace text. Let's say we have a string that contains the phrase "Hello world!" and we want to replace the word "world" with "universe". Our code would look like this:

```PHP
$string = "Hello world!";
$new_string = preg_replace("/world/", "universe", $string);
echo $new_string;
```

The output of this code would be "Hello universe!", as expected. Let's break down the `preg_replace()` function and its parameters:

- The first parameter is the regular expression pattern, surrounded by forward slashes (`/`). In this case, we are simply searching for the word "world".
- The second parameter is the replacement text, in this case "universe".
- The third parameter is the original string which we are performing the search and replace on.

Regular expressions can also be used to perform more complex searches, such as searching for specific patterns or characters within a string. Let's look at an example of how to replace all numbers in a string with the letter "x":

```PHP
$string = "I have 3 cats and 2 dogs.";
$new_string = preg_replace("/[0-9]/", "x", $string);
echo $new_string;
```

The output of this code would be "I have x cats and x dogs.". Here, the regular expression pattern is set to match any number between 0-9, and replaces it with the letter "x".

## Deep Dive

There are many different ways to use regular expressions in PHP for searching and replacing text. They allow for advanced and precise search patterns to be created, making the task of finding and replacing text much easier.

One important aspect to keep in mind when using regular expressions is the use of special characters. These characters have a specific meaning in a regular expression, and must be escaped with a backslash (\) if they are meant to be taken literally. For example, the period character (.) is a special character that matches any single character in a regular expression. If we want to search for a period in a string, we must escape it like this: `/\./`.

Another useful feature of regular expressions is the use of modifiers. These are additional characters that can be added at the end of a pattern to specify how the search should be performed. For example, using the `g` modifier will perform a global search, meaning all occurrences of the pattern will be replaced, rather than just the first one.

## See Also

- [PHP Manual: Regular Expressions](https://www.php.net/manual/en/regexp.reference.php)
- [Regex101](https://regex101.com/): a helpful tool for testing and creating regular expressions
- [Mastering Regular Expressions](https://www.amazon.com/dp/0596528124/): a comprehensive guide to mastering regular expressions