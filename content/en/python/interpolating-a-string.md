---
title:                "Interpolating a string"
html_title:           "Python recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation is a method for substituting values into placeholders within a string. Programmers can use this process to inject dynamically altered values into strings, making it a breeze to construct and manage sophisticated string formats.

## How to:
Here's how you can use Python’s built-in f-string method for string interpolation. The format is `{variable_name}` within the string.

```Python
# Define variables
name = 'John'
age = 30

# Interpolation using f-string
print(f"My name is {name}, and I am {age} years old.")

# Output:
# "My name is John, and I am 30 years old."
```
You can even do computations within `{...}`.

```Python
n = 5
m = 10

print(f"The sum of {n} and {m} is {n+m}.")

# Output:
# "The sum of 5 and 10 is 15."
```
## Deep Dive
Historically, Python offered the `%` operator and `str.format()` method for string formatting. The f-string method was introduced in Python 3.6 to make string formatting more intuitive and readable. 

Although older methods still work, the f-string method is more pythonic, meaning it aligns with the Python community's best practices for clarity and efficiency.

Implementation-play: Python’s f-string string interpolation undergoes the process at runtime, meaning interpolation expressions get evaluated when the program runs, adding extra flexibility.

## See Also
[Official Python Docs on String Interpolation](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)

[Python String Interpolation Tutorial](https://www.programiz.com/python-programming/string-interpolation)

[String formatting in Python](https://realpython.com/python-f-strings/#f-strings-a-new-and-improved-way-to-format-strings-in-python)