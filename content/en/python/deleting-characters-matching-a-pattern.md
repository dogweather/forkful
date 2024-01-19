---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is the practice of removing certain characters from a string based on a given pattern through pattern recognition mechanisms. Programmers do this primarily for data cleansing, where unnecessary or harmful data needs to be discarded.

## How to:
You can use the `re` module in Python to delete characters matching a pattern. Let's say you want to delete all the digits in a string. Here's how to do it:

```Python
import re

str = "Hello123"
str = re.sub(r'\d', '', str)
print(str)
```

When you run this, you'll see:

```Python
Hello
```

The `re.sub` function finds all matches to the given pattern (`\d` which matches any digit) and replaces them with the empty string.

## Deep Dive

The technique of deleting characters matching a pattern was introduced for data manipulation purposes. A key historical use has been in data preprocessing to clean up human-generated text that might have inconsistencies.

Alternatives to using the `re` module for this purpose include using built-in Python string functions or list comprehensions, but these might not offer the flexibility and power of regular expression matching.

The implementation of pattern deletion in Python is pretty straightforward. The `re` module compiles a pattern into a regular expression object, which is then used to match against strings.

## See Also

For more on Python's `re` module and regular expressions:
- Python `re` module: https://docs.python.org/3/library/re.html
- Regex Tutorial: https://www.w3schools.com/python/python_regex.asp
- Advanced String Manipulation: https://realpython.com/python-string-split-concatenate-join/