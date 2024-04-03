---
date: 2024-01-20 17:43:02.363431-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:59.693938-06:00'
model: gpt-4-1106-preview
summary: .
title: Deleting characters matching a pattern
weight: 5
---

## How to:
```Python
import re

# Example string
text = "Hello, World! 1234"

# Remove all digits
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # Output: "Hello, World! "

# Remove punctuation
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # Output: "Hello World 1234"

# Remove vowels
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # Output: "Hll, Wrld! 1234"
```

## Deep Dive
The practice of deleting characters matching a pattern in text has deep roots in computer science, tracing back to early Unix tools like `sed` and `grep`. In Python, the `re` module provides this capability, leveraging regular expressions—a powerful and versatile tool for text processing.

Alternatives to the `re` module include:
- String methods like `replace()` for simple cases.
- Third-party libraries like `regex` for more complex patterns and better Unicode support.

Under the hood, when you use `re.sub()`, the Python interpreter compiles the pattern into a series of bytecodes, processed by a state machine that performs pattern-matching directly on the input text. This operation can be resource-intensive for large strings or complex patterns, so performance considerations are crucial for big data processing.

## See Also
- [Python `re` module documentation](https://docs.python.org/3/library/re.html): Official docs for regular expressions in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): A comprehensive guide to regular expressions.
- [Real Python tutorial on regex](https://realpython.com/regex-python/): Real-world applications of regular expressions in Python.
