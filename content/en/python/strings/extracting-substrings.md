---
date: 2024-01-20 17:46:15.524985-07:00
description: "How to: Historically, the concept of string manipulation, including\
  \ substring extraction, was crucial in early programming languages such as C, where\
  \ it\u2026"
lastmod: '2024-04-05T21:53:35.379480-06:00'
model: gpt-4-1106-preview
summary: Historically, the concept of string manipulation, including substring extraction,
  was crucial in early programming languages such as C, where it was a more complex
  task involving pointers.
title: Extracting substrings
weight: 6
---

## How to:
```Python
# Using slice notation
text = "Python rocks!"
substring = text[7:12]
print(substring)  # Output: rocks

# Using the slice() function
slice_object = slice(7, 12)
print(text[slice_object])  # Output: rocks

# Using str.split() and accessing the element
parts = text.split()
print(parts[1])  # Output: rocks!
```

## Deep Dive
Historically, the concept of string manipulation, including substring extraction, was crucial in early programming languages such as C, where it was a more complex task involving pointers. With Python, the simplicity is dialed up to eleven - more intuitive and less error-prone.

Python provides multiple alternatives for extracting substrings. While the examples used slice notation which is super direct, methods like `split()` can be handy when you're dealing with delimiters or whitespace.

Under the hood, Python strings are arrays of bytes representing Unicode characters. But unlike arrays in other languages, Python strings are immutable, which means you can't change them after creation. This aspect is essential when understanding why substring operations don't modify the original string but instead create a new one.

## See Also
- The Python documentation on string methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- An article on more Python string operations: https://realpython.com/python-strings/
- W3Schools' Python string slicing tutorial: https://www.w3schools.com/python/python_strings_slicing.asp
