---
date: 2024-01-20 17:35:25.179967-07:00
description: "Concatenating strings means sticking them end-to-end to make a new string.\
  \ It's like string Lego. We do this to build up text; think usernames, error\u2026"
lastmod: '2024-03-13T22:44:59.700984-06:00'
model: gpt-4-1106-preview
summary: "Concatenating strings means sticking them end-to-end to make a new string.\
  \ It's like string Lego. We do this to build up text; think usernames, error\u2026"
title: Concatenating strings
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings means sticking them end-to-end to make a new string. It's like string Lego. We do this to build up text; think usernames, error messages, and dynamic content.

## How to:
Let's smash some strings together.

```python
first_name = "Charlie"
last_name = "Brown"
full_name = first_name + " " + last_name  # Classic concatenation with a space
print(full_name)
```
Output: `Charlie Brown`

Using `join()` for a list of words:

```python
words = ["Hello", "world!"]
sentence = " ".join(words)
print(sentence)
```
Output: `Hello world!`

F-String (since Python 3.6):

```python
user = "snoopy"
action = "flying"
log_message = f"{user} is {action} his doghouse"
print(log_message)
```
Output: `snoopy is flying his doghouse`

## Deep Dive
Concatenation has been a fundamental string operation since the dawn of programming. Remember, Python treats strings as immutable, so every concatenation creates a new string.

Once, the plus (`+`) was all we had. Not efficient for multiple strings, as it could lead to memory bloat and slow performance. Cue the `join()` method—more memory-friendly, especially for fusing a series of strings.

F-Strings, introduced in Python 3.6, are a game changer. They're readable and fast and allow expression evaluation within string literals—`f"{variable}"`. They're the go-to for a modern Pythonista, blending functionality and efficiency.

## See Also
- [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- [Python String Formatting Best Practices](https://realpython.com/python-f-strings/)
