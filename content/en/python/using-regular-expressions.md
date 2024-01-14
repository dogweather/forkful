---
title:    "Python recipe: Using regular expressions"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

If you're new to programming, you may have heard of regular expressions but might not be sure what they are or why they are useful. Regular expressions, also known as regex, are a tool used to search and manipulate text based on patterns. They allow for more powerful and efficient data processing by enabling you to extract, replace, or modify specific parts of a text string. Regular expressions are commonly used in text-based tasks such as data validation, web scraping, and data cleaning.

## How To

To use regular expressions in Python, you'll need to import the `re` module. Let's say we have a string with a phone number in it, and we want to extract just the numbers:

```Python
import re

string = 'My phone number is (123) 456-7890'
numbers = re.findall(r'\d+', string)

print(numbers)
```
Output: 
```
['123', '456', '7890']
```

In the above example, we used the `re.findall()` function to search for all occurrences of one or more digits in the string. The `r` before the regex pattern signifies a raw string which ensures that backslashes are not treated as escape sequences. We can also use regular expressions to replace certain parts of a string, like punctuation:

```Python
new_string = re.sub(r'[^\w\s]', '', string)

print(new_string)
```
Output:
```
My phone number is 123 456789
```

In this example, we used `re.sub()` to replace all non-alphanumeric characters with an empty string. There are many other useful functions in the `re` module, such as `re.search()`, `re.match()`, and `re.split()`. Experimenting with different regex patterns and functions can help you understand the power and versatility of regular expressions.

## Deep Dive

Regular expressions can use special characters and metacharacters to define patterns. Some of the most commonly used ones are:

- `.` matches any single character except a new line.
- `*` matches zero or more occurrences of the previous character.
- `+` matches one or more occurrences of the previous character.
- `?` matches zero or one occurrence of the previous character.
- `^` indicates the start of a string.
- `$` indicates the end of a string.

You can also use character classes such as `\d` for digits, `\w` for alphanumeric characters, and `\s` for whitespace. These metacharacters can also be used in conjunction with each other to create more complex patterns. Understanding these characters and how they work is essential for mastering regular expressions.

## See Also
- [Regular Expressions in Python](https://www.tutorialspoint.com/python/python_reg_expressions.htm)
- [re Module Documentation](https://docs.python.org/3/library/re.html)
- [Regex cheat sheet](https://www.rexegg.com/regex-quickstart.html)