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
Capitalizing a string in Python refers to converting the first letter of each word in a string to uppercase, while the remaining letters are converted to lowercase. Programmers often do this to standardize the formatting of strings and make them more readable.

## How to:
To capitalize a string in Python, you can use the built-in function `capitalize()`. This function takes the string as its argument and returns a new string with the first letter of each word capitalized.

```Python
# Example string
my_string = "this is an example string"
# Capitalize the string
new_string = my_string.capitalize()
print(new_string) # Output: This is an example string
```

You can also use string slicing to capitalize a string in Python. In this technique, you first convert the string to lowercase using the `lower()` function, then capitalize the first letter of the string using the `upper()` function, and finally concatenate it with the rest of the string.

```Python
# Example string
my_string = "this is an example string"
# Capitalize the string
capitalized_string = my_string[0].upper() + my_string[1:].lower()
print(capitalized_string) # Output: This is an example string
```

## Deep Dive:
Capitalizing strings has been a common practice in programming since the early days. It is often used for formatting purposes or to make strings more human-readable. In some programming languages, such as Java and C++, there are specific functions for capitalizing strings, but in Python, you can easily achieve this by using the `capitalize()` function or the string slicing technique.

There are a few alternatives to capitalizing strings in Python, such as using regular expressions or third-party libraries like `titlecase` or `pytitlecase`. However, these options may require more code and may not always produce the desired result.

The `capitalize()` function in Python uses the `unicode` module internally to handle non-ASCII characters. This ensures accuracy and consistency in string capitalization, regardless of the encoding used. It also has special handling for apostrophes and other punctuation marks.

## See Also:
- [Python String capitalize() method](https://www.programiz.com/python-programming/methods/string/capitalize)
- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)