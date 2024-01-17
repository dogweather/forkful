---
title:                "Extracting substrings"
html_title:           "Python recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the process of obtaining a smaller section of characters from a larger string. Programmers often do this in order to manipulate or analyze specific parts of a larger string without having to work with the entire string. This can make coding tasks more efficient and organized.

## How to:
```Python
# To extract a substring using square brackets
my_string = "Hello World"
print(my_string[0:5]) # Output: Hello

# To extract a substring using the slice() function
print(my_string[slice(6, 11)]) # Output: World

# Extracting a substring with negative indexing
print(my_string[-5:-1]) # Output: Worl

# Extracting a substring with a step
print(my_string[::2]) # Output: HloWrd
```

## Deep Dive:
Extracting substrings has been a vital technique in computer programming since the early days of string manipulation. Before the introduction of built-in string methods, programmers used to write their own functions to extract substrings. Nowadays, there are alternative methods in Python such as using the re library for regular expressions or splitting strings by a specific delimiter.

When extracting a substring, the syntax is [start_index:stop_index:step]. The start_index is inclusive while the stop_index is exclusive. This means that the character located at the stop_index will not be included in the extracted substring. If the step is not specified, it will default to 1. Negative indexes can also be used to extract substrings from the end of the string. If the step is negative, the substring will be reversed.

## See Also:
- [Python String Methods](https://www.geeksforgeeks.org/python-string-methods-set-1-find-rjust-split-replace/)
- [Regular Expressions in Python](https://www.geeksforgeeks.org/regular-expressions-python-examples-set-1/)
- [Python Slicing](https://www.geeksforgeeks.org/python-list-slicing/)