---
date: 2024-01-20 17:47:50.557306-07:00
description: "How to: Historically, the `len()` function has been Python's go-to way\
  \ to find a string's length. It's elegant and quick. Underneath, Python strings\
  \ are\u2026"
lastmod: '2024-04-05T21:53:35.381738-06:00'
model: gpt-4-1106-preview
summary: Historically, the `len()` function has been Python's go-to way to find a
  string's length.
title: Finding the length of a string
weight: 7
---

## How to:
```python
# Simple usage of len() function
my_string = "Hello, World!"
length = len(my_string)
print(length)  # Output: 13

# Length in a loop
for i in range(len(my_string)):
    print(my_string[i], end='')  # Outputs: Hello, World!
print()  # For newline

# Combine string length with other operations
if len(my_string) > 10:
    print("It's a long string!")  # Output: It's a long string!
```

## Deep Dive
Historically, the `len()` function has been Python's go-to way to find a string's length. It's elegant and quick. Underneath, Python strings are arrays of bytes representing Unicode characters, and `len()` counts those. The function works not just with strings but with any iterable.

Alternatives? Well, not commonly used for strings, but you could loop through a string and count characters manually—unwieldy and inefficient. Before Unicode support, the length of a string was sometimes different from its memory size, but since Python 3's strings are Unicode-native, the `len()` accurately represents the number of characters.

Implementation-wise, Python strings are objects with metadata, including length, so `len()` is actually an O(1) operation—constant time, regardless of string size. That's like snapping your fingers and getting an answer.

## See Also
- Python documentation for `len()`: https://docs.python.org/3/library/functions.html#len
- Unicode and String Encoding in Python: https://docs.python.org/3/howto/unicode.html
- Python's time complexity for built-in types: https://wiki.python.org/moin/TimeComplexity
