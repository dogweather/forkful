---
title:                "Capitalizing a string"
html_title:           "Python recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

To capitalize a string in Python means to make the first character uppercase and the rest lower. It's often done for formatting text or for making comparisons case-insensitive.

## How to:

Python provides built-in string methods for capitalizing strings: `capitalize()`, `lower()`, and `upper()`. Here's how you do it:

```Python
# using capitalize()
text = "hello world"
text = text.capitalize()
print(text)  # Output: Hello world

# changing all to uppercase
text = "hello world"
text = text.upper()
print(text)  # Output: HELLO WORLD

# changing all to lowercase
text = "HELLO WORLD"
text = text.lower()
print(text)  # Output: hello world
```

## Deep Dive

Historically, text-capitalization was vital in programming to handle variations in user input. With capitalized format, we have a standardized form, reducing errors due to differences in case.

While Python's `capitalize()`, `upper()`, and `lower()` methods are the most common, you can also use `title()` to capitalize the first letter of each word:

```Python
text = "hello world"
text = text.title()
print(text)  # Output: Hello World
```

The built-in methods are implemented in C. These methods traverse through each character in the input string, changing them to their equivalent uppercase or lowercase ASCII value. For non-ASCII characters, it uses Unicode mappings.

## See Also:

- Python's Official Documentation on String Methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- On Efficient String Concatenation: https://waymoot.org/home/python_string/
- A Tutorial on Python Strings and Their Built-in Functions: https://realpython.com/python-strings/