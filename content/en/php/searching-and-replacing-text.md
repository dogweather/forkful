---
title:                "Searching and replacing text"
html_title:           "PHP recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

In the world of web development, text is a crucial element. Whether it's a simple blog post or a complex codebase, there are times when we need to make changes in the text. In such situations, searching and replacing text becomes a necessary task. It helps us save time and effort by quickly replacing multiple instances of text with just one command. 

## How To

To search and replace text in PHP, we can use the `str_replace()` function. This function takes in three parameters: the text we want to replace, the new text we want to insert, and the original string. Let's take a look at a simple example:

```PHP
// Original string
$text = "Hello world, welcome to the PHP world!";

// Replacing "world" with "universe"
$new_text = str_replace("world", "universe", $text);

// Output: Hello universe, welcome to the PHP universe!
```

As you can see, the `str_replace()` function has replaced all instances of "world" with "universe" in our original string. However, if we only want to replace a specific instance, we can use the optional fourth parameter for the number of replacements we want to make. Let's see it in action:

```PHP
// Original string
$text = "I love PHP, it's one of my favorite languages!";

// Replacing first instance of "PHP" with "JavaScript"
$new_text = str_replace("PHP", "JavaScript", $text, 1);

// Output: I love JavaScript, it's one of my favorite languages!
```

In this example, only the first instance of "PHP" is replaced with "JavaScript" because we specified that we only want one replacement to occur. This can be useful when dealing with large strings or when we want to make specific changes.

## Deep Dive

The `str_replace()` function in PHP is case-sensitive, meaning that it will only replace text with the same case as the original string. However, if we want to ignore the case while searching and replacing, we can use the `str_ireplace()` function. It works in the same way as `str_replace()`, but it ignores case differences.

Another useful function for searching and replacing text in PHP is `preg_replace()`. It uses regular expressions to search and replace text, giving us more flexibility in the types of replacements we can make. Let's see an example:

```PHP
// Original string
$text = "I have 4 cats and 1 dog.";

// Replacing numbers with "a"
$new_text = preg_replace("/\d/", "a", $text);

// Output: I have a cats and a dog.
```

In this example, we used a regular expression to replace all numbers with the letter "a". This can be useful when, for example, we want to hide sensitive information like credit card numbers or phone numbers. Regular expressions can be complex, but they can also be very powerful in text manipulation.

## See Also

- [PHP Documentation on str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Documentation on preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions in PHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)