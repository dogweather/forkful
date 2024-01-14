---
title:                "PHP recipe: Deleting characters matching a pattern"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

There are times when we may need to delete certain characters from a string that matches a specific pattern in our PHP code. This can be a useful technique in cleaning up data or manipulating strings for various purposes. In this blog post, we will explore how to delete characters matching a pattern in PHP.

## How To

To delete characters matching a pattern in PHP, we will use the `preg_replace()` function. This function searches a string for a specific pattern and replaces it with a different string. Let's take a look at an example:

```PHP
$text = "Hello, [world]!";
$clean_text = preg_replace("/[^\w\s]/", "", $text);

echo $clean_text;
```

In this example, we have a string with special characters such as brackets and an exclamation mark. We want to remove these characters and only keep letters, numbers, and spaces. The `preg_replace()` function takes two parameters - the pattern to search for and the replacement string. In our pattern, we use the brackets `[]` to specify the range of characters we want to keep, and the caret `^` to indicate negation. This means that all characters except for letters, numbers, and spaces will be replaced with an empty string. The output of this code will be:

```PHP
Hello world
```

We can also use regular expressions in our pattern to match more complex patterns. For example, let's say we want to remove all numbers from a string. We can use the `\d` pattern which matches any digit from 0-9. 

```PHP
$text = "I have 3 cats and 2 dogs";
$clean_text = preg_replace("/\d/", "", $text);

echo $clean_text;
```

The output of this code will be:

```PHP
I have  cats and  dogs
```

We can use this technique to remove any characters or patterns that we do not want in our strings.

## Deep Dive

When using regular expressions in our `preg_replace()` function, it's important to understand different modifiers that can affect the outcome. For example, the `i` modifier makes the pattern case-insensitive, `g` makes it search globally, and `m` makes it multi-line. You can read more about useful modifiers in the [PHP documentation](https://www.php.net/manual/en/reference.pcre.pattern.modifiers.php).

It's also worth mentioning that using regular expressions can be more resource-intensive than other string manipulation techniques. So if you are working with large amounts of data, it's important to keep this in mind and consider alternative solutions.

## See Also

- [PHP preg_replace() function documentation](https://www.php.net/manual/en/function.preg-replace.php)
- [Regex Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/php)
- [PHP string manipulation functions](https://www.w3schools.com/php/php_ref_string.asp)