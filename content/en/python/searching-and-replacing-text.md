---
title:                "Searching and replacing text"
html_title:           "Python recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Search and Replace: A Python Approach

## What & Why?
Searching and replacing text is a process of locating a specific sequence of characters in a string and substitifying it with another. Programmers use this for data cleaning, formatting, and manipulation.

## How To:

Here's a simple example using `str.replace()`:
```Python
def replace_text(original_text, target, replacement):
    return original_text.replace(target, replacement)

original_text = "Hello, World!"
print(replace_text(original_text, "World", "Python"))
```
Output:
```
Hello, Python!
```
In this small script, we have a function replace_text that takes an original text, a target substring, and a replacement string as arguments. It then uses the Python built-in str.replace() method to replace all instances of the target in the original text with the replacement.

## Deep Dive:
Historically, Find and Replace was a mainstay of text editing and word processing long before programming. Modern programming languages have refined and expanded it.

In Python, alternatives to `str.replace()` exist. `re.sub()`, a method from the `re` (Regular Expression) library, provides more flexibility, like case-insensitive replacement.

Here's an implementation of case-insensitive search & replace using `re.sub()`. Note the use of the `re.I` flag.

```Python
import re

def insen_replace(original_text, target, replacement):
    return re.sub(target, replacement, original_text, flags=re.I)

original_text = "Hello, World!"
print(insen_replace(original_text, "world", "Python"))
```
Output:
```
Hello, Python!
```
## See Also:
For more information on Python's `replace()`, check out the official Python documentation: [Python Doc](https://docs.python.org/3/library/stdtypes.html#str.replace).
For an introduction to Python's Regular Expressions (re) library: [Python Regex](https://docs.python.org/3/library/re.html).