---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Substring extraction refers to getting a smaller part of a string in Python. It's a must-know because it helps handle and manipulate data efficiently. 

## How to:

In Python, we use string slicing to extract substrings:

```python
# Here's an example:

s = "Hello, World!"
substring = s[7:12]
print(substring)

# Output: World
```
In this standing example, we extracted "World" by using index positions. Python slicing starts from '0' (for the first element).

## Deep Dive:

Historically, strings in Python were implemented as arrays, making substring extraction fast and efficient. There have been modifications, but we still use slicing because it's effective, quick, and readable. 

Python string slicing reads as follows: `s[start:stop:step]`. If `step` isn't mentioned, Python moves at a pace of 1.

If you find slicing difficult or not suitable for your condition, you can use Python's built-in `split()` function. However, it splits using spaces or another delimiters and returns a list of substrings.

```python
s = "Hello, World!"
substrings = s.split(',')
print(substrings)

# Output: ['Hello', ' World!']
```
It's essential that you remember Python uses zero-based indexing. This means that the first position in the string is `0`, not `1`. And don't forget, in Python, strings are immutable, which means they can't be modified once created.

Python treats strings as an array of individual characters – so all of the array functionality applies to strings too.

## See Also:

Check out these additional resources for more information about slicing and other operations on strings:

- [An A-Z of useful Python tricks](https://medium.freecodecamp.org/an-a-z-of-useful-python-tricks-b467524ee747)
- [Python Official Docs on Strings](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [A Guide to Python's Slice Notation](https://towardsdatascience.com/a-guide-to-pythons-slice-notation-58674ee8d63d)

Happy coding!