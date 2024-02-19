---
aliases:
- /en/python/using-regular-expressions/
date: 2024-02-03 19:03:03.774454-07:00
description: "Regular expressions (regex) are patterns used to match character combinations\
  \ in strings. Programmers utilize them for searching, editing, or manipulating\u2026"
lastmod: 2024-02-18 23:09:10.671557
model: gpt-4-0125-preview
summary: "Regular expressions (regex) are patterns used to match character combinations\
  \ in strings. Programmers utilize them for searching, editing, or manipulating\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in strings. Programmers utilize them for searching, editing, or manipulating text based on defined patterns, making them indispensable for tasks like data validation, parsing, or transformation.

## How to:
Using regex in Python involves the `re` module, which provides a set of functions to process text using regular expressions.

### Basic Pattern Matching
To search for a pattern in a string, use `re.search()`. It returns a match object when the pattern is found, else `None`.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("Pattern found!")
else:
    print("Pattern not found.")
```
Output:
```
Pattern found!
```

### Compiling Regular Expressions
For repeated use of the same pattern, compile it first with `re.compile()` for better performance.
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("Compiled pattern found!")
```
Output:
```
Compiled pattern found!
```

### Splitting Strings
To split a string at each match of a regex pattern, use `re.split()`.
```python
result = re.split("\s", "Python is fun")
print(result)
```
Output:
```
['Python', 'is', 'fun']
```

### Finding All Matches
To find all non-overlapping occurrences of a pattern, use `re.findall()`.
```python
matches = re.findall("n", "Python programming")
print(matches)
```
Output:
```
['n', 'n']
```

### Replacing Text
Use `re.sub()` to replace occurrences of a pattern with a new string.
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
Output:
```
Python is awesome
```

### Third-Party Libraries
While Python's built-in `re` module is powerful, third-party libraries like `regex` offer more features and enhanced performance. To use `regex`, install it via pip (`pip install regex`) and import it in your code.

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Found version: {match.group(1)}")
```
Output:
```
Found version: 3.8
```
