---
title:                "स्ट्रिंग को इंटरपोलेट करना"
html_title:           "Python: स्ट्रिंग को इंटरपोलेट करना"
simple_title:         "स्ट्रिंग को इंटरपोलेट करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
"String interpolation" is the process of creating a new string by inserting a value or variable into a predefined string. Programmers use string interpolation to dynamically create strings with changing values, rather than manually concatenating different strings together.

# How to:
To interpolate a string, you can use the "f-string" method in Python. This method allows you to insert values or variables inside curly braces within a string. Let's see an example:

```python
name = "John"
age = 25
greeting = f"Hello, my name is {name} and I am {age} years old."
print(greeting)
```

This will output: `Hello, my name is John and I am 25 years old.`

You can also use string interpolation with arithmetic operations, as shown in the example below:

```python
num1 = 10
num2 = 20
result = f"The sum of {num1} and {num2} is {num1 + num2}."
print(result)
```

This will output: `The sum of 10 and 20 is 30.`

# Deep Dive:
String interpolation has been around since the early days of programming. In the past, programmers used concatenation operators (`+` and `+=`) to create new strings, which was a laborious task. But with the introduction of f-strings in Python 3.6, string interpolation became much easier and more efficient.

There are also other methods to interpolate strings, such as using the `format()` function or the `%` operator. However, f-strings are considered the most preferred and efficient method in Python.

To use the f-string method, the string must start with the letter `f`, followed by the string itself. The values or variables that need to be inserted can be placed inside curly braces, using the `format()` or `%` method, or even using object methods such as `capitalize()` or `upper()`, as shown in the example below:

```python
name = "emma"
greeting = f"Hello, {name.capitalize()}!"
print(greeting)
```

This will output: `Hello, Emma!`

# See Also:
- [Official Python documentation on strings](https://docs.python.org/3/library/string.html)
- [f-strings vs. format() vs. % operator](https://realpython.com/python-f-strings/#comparing-f-strings-and-strformat-vs-operator)
- [Alternative string interpolation methods in Python](https://www.tutorialspoint.com/python/string_interpolation_in_python.htm)