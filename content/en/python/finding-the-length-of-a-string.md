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

## What & Why?

Finding the length of a string means determining the number of characters in a given string. This is a common task for programmers, as it allows for manipulating and analyzing text data in various ways. Whether it's for data validation, formatting, or analysis purposes, knowing the length of a string is essential in many programming applications.

## How to:

Calculating the length of a string in Python is a simple task. Use the `len()` function and pass the string as an argument to it. For example:

```Python
string = "Hello, World!"
print(len(string))
```

This will output `13`, as the string contains 13 characters. Additionally, you can also use the `len()` function on other data types, such as lists, tuples, and dictionaries.

```Python
list = [1, 2, 3, 4, 5]
print(len(list))

tuple = (6, 7, 8, 9)
print(len(tuple))

dictionary = {'a': 1, 'b': 2, 'c': 3}
print(len(dictionary))
```

The output for these examples would be `5`, `4`, and `3` respectively.

## Deep Dive:

Calculating the length of a string is a relatively simple concept, but it has evolved over time. In the early days of programming, the length of a string was determined by finding the string's null terminator, which is a character used to indicate the end of a string. However, with the introduction of high-level programming languages, this process has become more efficient and less complicated.

While the `len()` function is the most common way of finding the length of a string in Python, there are other approaches as well. For example, using a for loop to iterate through each character in the string and incrementing a counter variable could also achieve the same result. However, this method is less efficient and recommended only for learning purposes.

Internally, the `len()` function uses a hidden attribute of an object called `__len__()` to calculate the length of a string and return it. All built-in objects in Python have this attribute, allowing them to be used with the `len()` function.

## See Also:

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp) - other useful methods for manipulating strings.
- [Python Documentation on len()](https://docs.python.org/3/library/functions.html#len) - official documentation for the `len()` function in Python.
- [Python Strings](https://realpython.com/python-strings/) - a comprehensive guide on strings in Python.