---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

String concatenation is about joining two or more strings together. Programmers do it to create dynamic strings, useful in numerous situations like generating messages or crafting URLs.

## How to: 

Python supports string concatenation in many ways. The most straightforward method is using the plus (+) operator.

```python
# Example 1: Using the '+' operator
str_1 = "Hello,"
str_2 = " World!"
concatenated_str = str_1 + str_2
print(concatenated_str)  # Output: "Hello, World!"
```

Another handy tool for string concatenation is the `join()` method.

```python
# Example 2: Using the 'join()' method
str_list = ["Hello,", " World!"]
concatenated_str = ''.join(str_list)
print(concatenated_str)  # Output: "Hello, World!"
```

## Deep Dive

Historically, in the earlier versions of Python, the '+' operator was the go-to method for string concatenation. But it has a drawback. Every '+' operation creates a new string, making it less efficient for multiple concatenations.

The 'join()' method, in contrast, is more efficient as it precalculates the length of the resulting string and then creates it, making it faster, especially for larger strings.

There are alternatives, however, including string interpolation methods like `f-strings` (Python 3.6 onwards) and `format()`.

```python
# Example 3: Using 'f-strings'
str_1 = "Hello,"
str_2 = " World!"
concatenated_str = f"{str_1}{str_2}"
print(concatenated_str)  # Output: "Hello, World!"
```

```python
# Example 4: Using 'format()'
str_1 = "Hello,"
str_2 = " World!"
concatenated_str = "{}{}".format(str_1, str_2)
print(concatenated_str)  # Output: "Hello, World!"
```

Performances vary depending on situations. For small strings or fewer concatenation operations, differences are negligible. But for large strings or more concatenations, 'join()' tends to outperform '+', while f-strings are generally faster than 'format()'.

## See Also:

1. [Python Docs: Common string operations:](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. [Python Docs: String Formatting](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)
3. [Real Python: String Concatenation](https://realpython.com/python-string-concatenation/)