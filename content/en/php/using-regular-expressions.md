---
title:                "PHP recipe: Using regular expressions"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful and commonly used tool in programming. They allow developers to search for patterns within strings, making tasks such as data validation, parsing, and manipulating much easier. So, if you want to improve your coding efficiency and make your life easier, learning regular expressions is a must.

## How To

Regular expressions are written using specific characters and symbols to represent patterns. Let's take a look at a simple regex example to find all email addresses in a string. 

```PHP 
$string = "My email is john@example.com and my friend's email is jane@hotmail.com";
$pattern = "/\S+@\S+\.\S+/";
preg_match_all($pattern, $string, $matches);
print_r($matches[0]);
```

The output of this code would be:

```
Array
(
    [0] => john@example.com
    [1] => jane@hotmail.com
)
```

Let's break down the pattern we used in the code:

- `\S` represents any non-whitespace character
- `+` means that the previous character or group can appear one or more times
- `@` represents the `@` symbol
- `\.` represents the `.` symbol (notice the `\` is used to escape the special meaning of the `.`)
- `$matches[0]` will contain all the matches, while `$matches[1]` will contain only the email addresses without the `@` and `.` symbols.

This is just a basic example, but as you can see, regular expressions can be a powerful and efficient way to find patterns in strings. There are many other symbols and methods that can be used, so it's important to refer to documentation and practice using them.

## Deep Dive

Regular expressions can be used in various programming languages, including PHP. In PHP, the `preg_match()` and `preg_match_all()` functions are used to perform regex operations. These functions take three parameters - the pattern, the subject string, and an optional variable to store the matches.

There are also different modifiers that can be added to the end of a regex pattern to specify things such as case-insensitivity, multiline matching, and more. It's important to understand different modifiers and their effects on the pattern match.

Another thing to keep in mind when using regular expressions is to be aware of performance. While they are great for certain tasks, they can also cause performance issues if not used correctly. It's best to test and optimize your regex patterns to ensure they are running efficiently.

## See Also

- [PHP Official Documentation on Regular Expressions](https://www.php.net/manual/en/book.pcre.php)
- [Regex101 - Tool for testing and building regular expressions](https://regex101.com/)
- [Mastering Regular Expressions - Book by Jeff Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)