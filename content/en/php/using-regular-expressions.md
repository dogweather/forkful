---
title:                "Using regular expressions"
html_title:           "PHP recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, also known as regex, are special sequences of characters used to find and manipulate patterns in text. They are commonly used in programming to search, replace, and validate text data. This makes it easier and more efficient for programmers to process large amounts of data, especially when dealing with complex patterns.

## How to:
Using regular expressions in PHP is simple and straightforward. The ```preg_match()``` function is used to search for a specific pattern in a string, while the ```preg_replace()``` function is used to replace a pattern with a specified string. Let's take a look at some examples:

#### Searching for a Specific Pattern
In this example, we want to find all email addresses in a given string. We can use the ```preg_match()``` function along with a regular expression to do this.

```
$email = "john@example.com, jane@example.net";

// Using preg_match() to find email addresses
preg_match("/[\w.-]+@[\w.-]+\.[a-z]{2,3}/", $email, $matches);

// $matches[0] will contain the first match
echo $matches[0]; // john@example.com 
```

#### Replacing a Pattern
In this example, we want to replace all instances of "apple" with "orange" in a given string. Again, we can use the ```preg_replace()``` function with a regular expression to achieve this.

```
$text = "I love apples, apples are my favorite fruit.";

// Using preg_replace() to replace "apple" with "orange"
$new_text = preg_replace("/apple/", "orange", $text);

echo $new_text; // I love oranges, oranges are my favorite fruit.
```

## Deep Dive:
Regular expressions have been around since the 1950s and have become an integral part of many programming languages including PHP. They were first used in Unix tools like grep, sed, and awk for text processing and have since been adopted by many other languages.

While regular expressions are great for text processing, they do have their alternatives. These include using string functions like ```strpos()``` and ```str_replace()```, or using PHP's built-in ```filter_var()``` function with the appropriate filter.

When implementing regular expressions in PHP, it's important to use delimiters. These act as start and end markers for the regular expression and are usually a forward slash (/), but any non-alphanumeric character can be used as long as it's consistently used throughout the expression. 

## See Also:
- [PHP Manual on Regular Expressions](https://www.php.net/manual/en/regexp.reference.php)
- [Online regex tester](https://regex101.com/)