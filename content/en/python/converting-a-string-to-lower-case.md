---
title:                "Converting a string to lower case"
html_title:           "Python recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case in Python means changing all the letters in the string to their lowercase counterparts. Programmers often do this in order to standardize string inputs and make it easier to compare and manipulate them.

## How to:
```Python
# using the lower() method
my_string = "Hello World"
lower_string = my_string.lower()
print(lower_string) # output: hello world

# using the lower() function
my_string = "Hello World"
lower_string = lower(my_string)
print(lower_string) # output: hello world
```

## Deep Dive:
Converting strings to lower case has been a common practice in computer programming since the early days of computing. In Python, there are two ways to accomplish this task: using the built-in lower() method or the lower() function from the string module. The method is called on a specific string, while the function is called on the entire string input. Both methods return a new string with all letters converted to lower case. Other alternatives to converting strings to lower case include using regular expressions or custom functions.

## See Also:
- [Python String Methods Documentation](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python String Module Documentation](https://docs.python.org/3/library/string.html)
- [Regular Expressions in Python](https://www.geeksforgeeks.org/regular-expression-python/)