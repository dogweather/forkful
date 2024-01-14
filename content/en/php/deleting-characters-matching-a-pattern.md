---
title:    "PHP recipe: Deleting characters matching a pattern"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters is a common task in programming, but what about deleting characters that match a specific pattern? This technique can be useful for data cleaning, removing unwanted characters from user inputs, or even creating custom filters for text-based data. In this blog post, we will explore how to delete characters matching a pattern in PHP.

## How To

Before diving into the code, let's define what we mean by "characters matching a pattern". In PHP, a pattern can be defined using regular expressions, also known as regex. Regex allows us to specify a set of characters to match in a string.

Now, let's see how we can use regex to delete characters matching a certain pattern in PHP:

```PHP
// Create a string with unwanted characters
$input = "Hello! How are you?";

// Use preg_replace() function to remove non-alphanumeric characters
$output = preg_replace("/[^a-zA-Z0-9 ]/i", "", $input); 

// Output: Hello How are you
echo $output;
```

In the example above, we used the `preg_replace()` function with a regular expression pattern to remove all non-alphanumeric characters (such as `!` and `?`) from the string. The `i` at the end of the pattern means case-insensitive, so both uppercase and lowercase characters will be matched.

You can customize the pattern to fit your specific needs. For instance, if you only want to remove numbers, the pattern would be `"/[0-9]/"`. Or, if you want to remove all special characters, the pattern would be `"/[^a-zA-Z0-9]/i"`.

## Deep Dive

Now that we know how to use regex to delete characters matching a pattern, let's take a closer look at the `preg_replace()` function. It takes three parameters: the pattern, the replacement, and the input string.

The first parameter, pattern, is where we specify the characters we want to match using regex. The second parameter, replacement, is what we want to replace the matched characters with. In our previous example, we left the replacement parameter empty, so the matched characters were simply deleted.

The third parameter, input, is the string we want to search and modify. This can be a variable or a string literal.

Apart from the `preg_replace()` function, PHP also has other built-in functions for pattern matching and string replacement, such as `preg_match()` and `str_replace()`. These functions may have different syntaxes, but the underlying concept is the same - using patterns to match and manipulate strings.

## See Also

1. PHP manual on regular expressions: <https://www.php.net/manual/en/book.regex.php>
2. Regex tutorials and cheat sheets: <https://www.regular-expressions.info/>, <https://www.rexegg.com/regex-quickstart.html>

Deleting characters matching a pattern can be a useful skill to have in your PHP programming toolbox. Armed with the knowledge of regex, you can manipulate strings in more advanced and efficient ways. Try it out in your next project and see how it can improve your code!