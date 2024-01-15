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

## Why

Deleting characters matching a pattern can be a useful task in a variety of scenarios. It can help with data cleaning and manipulation, text parsing, and even security checks. By learning how to do this in PHP, you can save time and streamline your coding process.

## How To

To delete characters matching a pattern in PHP, you can use the `preg_replace()` function. This function takes three parameters: the pattern to match, the replacement string, and the input string. Let's take a look at a simple example:

```PHP
$input = "Hello123World";
$pattern = '/[0-9]/'; // matches any number between 0 and 9
$output = preg_replace($pattern, '', $input);

echo $output; // Output: HelloWorld
```

The `$output` variable will now contain the input string without any numbers in it. In this case, we used a regular expression as the pattern, but you can also use plain text strings. The `preg_replace()` function will replace all matches in the input string with the replacement string.

You can also use the "global" modifier `/g` to replace all matches, instead of just the first one. For example:

```PHP
$input = "1 apple, 2 oranges, and 3 bananas";
$pattern = '/[0-9]+/g'; // matches any sequence of numbers
$output = preg_replace($pattern, '', $input);

echo $output; // Output: apple, oranges, and bananas
```

In this example, we used the `+` modifier to match one or more consecutive numbers. This allows us to replace all numbers in the input string.

## Deep Dive

Behind the scenes, `preg_replace()` uses the PCRE (Perl Compatible Regular Expressions) library to perform pattern matching and replacement. This library is also used in other programming languages like Python, JavaScript, and Ruby. It provides a powerful and flexible way to match and manipulate strings.

So far, we have only scratched the surface of what is possible with regular expressions and pattern matching in PHP. You can use modifiers, quantifiers, character classes, and more to create complex patterns for matching. It's worth taking the time to learn more about regular expressions and how to use them effectively.

## See Also

- [PHP documentation on preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PCRE documentation](https://www.pcre.org/)
- [Regular expressions tutorial](https://regexone.com/)