---
title:                "Deleting characters matching a pattern"
html_title:           "Python recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is the process of removing specific characters or sequences of characters from a given string. Programmers use this technique to manipulate strings and remove unwanted characters, making their code more efficient and streamlined.

## How to:

To delete characters matching a pattern in Python, you can use the built-in `re` (regular expression) library. This library allows you to use patterns and rules to search and replace strings in your code. Here is a simple example of using `re` to delete all numbers from a given string:

```Python
import re

text = "abc123def456ghi789"
pattern = "\d" # \d matches all digits
result = re.sub(pattern, "", text)
print(result) # Output: abcdefghi
```

You can use regex (regular expressions) to build more complex patterns and rules for deleting characters. For example, you can target specific sequences of characters or replace them with new characters.

```Python
import re

text = "Hello World!"
pattern = "o[a-z]+"
replacement = "O"
result = re.sub(pattern, replacement, text)
print(result) # Output: HellO World!
```

## Deep Dive:

The concept of deleting characters from a string has been around for a long time and has evolved with different programming languages. In Python, the use of regular expressions for string manipulation has made deleting characters a simple and effective process.

An alternative to using `re` for deleting characters is to use built-in string methods such as `replace()`. However, these methods have limitations in terms of flexibility and efficiency compared to using regular expressions. `re` allows for more complex patterns and rules to be used, making it a powerful tool for string manipulation.

When using `re` for deleting characters, it is important to understand the syntax and patterns used. The reference documentation for `re` provides detailed information on the different metacharacters and functions that can be used for matching and replacing strings.

## See Also:

- [Python re documentation](https://docs.python.org/3/library/re.html)
- [Regular expressions explained](https://www.regular-expressions.info/)
- [Python string methods](https://docs.python.org/3/library/stdtypes.html#string-methods)