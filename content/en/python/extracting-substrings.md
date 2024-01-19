---
title:                "Extracting substrings"
html_title:           "Python recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means pulling specific sections out from entire strings in Python. Programmers do this to filter, parse, or manipulate data efficiently.

## How To:

The simplest way to extract a substring in Python is by using slicing:

```Python
s = "Hello, World!"
print(s[0:5])  # Expected output: 'Hello'
```

Or, you can extract substrings using the split() method:

```Python
s = "Hello, World!"
print(s.split(", ")[0])  # Expected output: 'Hello'
```

## Deep Dive:

The concept of extracting substrings has been around almost as long as programming itself. Early languages like Fortran realized the importance of working with parts of strings, although the implementation was clunkier than what we have today.

You may also use functions such as find() and index() in combination with slicing to extract substrings.

```Python
s = "Hello, World!"
start = s.find("World")
print(s[start:])  # Expected output: 'World!'
```

Though, remember that the find() function will return -1 if the substring is not found. The index() function, however, raises a ValueError exception.

The Python substring methods are designed with efficiency in mind. Python strings are immutable; thus, these methods don't modify the original string but return a new string.

## See Also:

1. [Python Official Documentation on Strings](https://docs.python.org/3/tutorial/introduction.html#strings)
2. [Python Basics: String Manipulation](https://pythonbasics.org/string-manipulation/)
3. [Real Python: An Introduction to String Functions in Python 3](https://realpython.com/python-strings/)