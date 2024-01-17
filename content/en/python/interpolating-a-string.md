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

String interpolation in Python refers to the process of embedding variables or expressions within a string. This allows for the dynamic creation of strings based on the values of these variables or expressions. Programmers use string interpolation to make their code more concise and readable, as well as to reduce the amount of coding required for creating and formatting strings.

## How to:

String interpolation in Python is achieved using the string formatting operator % or the format() function. Let's see some examples:

```Python
# Using the % operator
age = 25
name = "John"
print("Hello, my name is %s and I am %d years old." % (name, age))
```
Output: Hello, my name is John and I am 25 years old.

```Python 
# Using the format() function
company = "ABC Corp"
employees = 100
print("The company {} has {} employees.".format(company, employees))
```
Output: The company ABC Corp has 100 employees.

In these examples, we have inserted the values of the variables ```name```, ```age```, ```company``` and ```employees``` into our strings using the % operator and the format() function, respectively.

## Deep Dive:

In older versions of Python (before 3.6), string interpolation was done using the % operator. However, in Python 3.6 and above, the recommended way of string interpolation is through f-strings. F-strings allow for even simpler string interpolation by directly formatting the string with the values of the variables or expressions enclosed in curly braces.

Besides f-strings, another alternative to string interpolation in Python is the string Template class from the string module. This allows for more advanced string interpolation techniques, such as substituting values from dictionaries or using placeholder values for missing variables.

In terms of implementation, string interpolation involves parsing the string and replacing the placeholders with the values of the variables or expressions. This is done by calling the __mod__() method in the case of % operator or the __format__() method for the format() function.

## See Also:

- Official Python documentation on [string formatting operations](https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting)
- Real Python's [guide on string interpolation](https://realpython.com/python-string-formatting/) for a more detailed explanation and examples of different string interpolation methods.