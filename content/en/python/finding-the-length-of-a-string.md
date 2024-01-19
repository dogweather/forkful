---
title:                "Finding the length of a string"
html_title:           "Python recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Exploring String Length in Python: Brevity and Simplicity Rule

## What & Why?
Determining the size of a string – the number of characters it contains – is a common task in Python. Seeing this count lets programmers handle large data, time-consuming operations, or user input efficiently.

## How to:
Finding the length of a string in Python is accomplished using the `len()` function. This function takes a string as its argument and returns the count of characters (including spaces) in it.
Here’s how you can do it:

```python
str = "Hello, World!"
print(len(str))  # Returns 13
```

The output will be `13`. The comma, space, and exclamation mark are all counted as characters.

## Deep Dive:

The `len()` function has been around since the birth of Python and is a built-in function, a testament to its practicality. It's simple, direct, and efficient, being O(1) complexity, meaning it takes constant time regardless of the string size.

In Python, strings are objects and have several built-in methods. These include `__len__()` - you can call `str.__len__()` to get the same result:

```python
str = "Hello, World!"
print(str.__len__())  # Returns 13
```

But it's more Pythonic to use `len(str)`. It's simpler and cleaner code.

The `len()` function is preferable over manually iterating through the string with a for-loop and incrementing a counter. Such an approach is more cumbersome, prone to errors from overlooking special characters or forgetting edge cases, and slower because it has an O(n) complexity.

## See Also:

For more about Python's string built-ins, see the [official documentation](https://docs.python.org/3/library/stdtypes.html#string-methods).

For more on computational complexity in Python, this [TimeComplexity Wiki](https://wiki.python.org/moin/TimeComplexity) sheds light.

For an insightful understanding of Python's `len()` function, check out this [Stackoverflow Discussion](https://stackoverflow.com/questions/2485466/pythons-len-function-performance).