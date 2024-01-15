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

## Why
Converting a string to lower case is a common task in programming, especially when working with user input. It helps to standardize the input and make it easier to compare and manipulate.

## How To
The process of converting a string to lower case in Python is simple and can be achieved in a few different ways. Let's go through two methods - using the `lower()` function and using the `casefold()` function.

```Python
# Method 1: Using the lower() function
text = "HELLO WORLD"
print(text.lower())  # Output: hello world

# Method 2: Using the casefold() function
text = "HELLO WORLD"
print(text.casefold())  # Output: hello world
```

As you can see, both methods produce the same output. The only difference is that `casefold()` is safer and more robust as it takes into account more characters, such as accented letters.

## Deep Dive
Behind the scenes, when we call either the `lower()` function or the `casefold()` function, the string is converted into its lowercase equivalent by using the Unicode standard. This means that not only English letters will be converted, but also any letters or characters from other languages that have lowercase equivalents.

Additionally, both functions return a new string, leaving the original string unchanged. This is important to keep in mind, especially if you want to store the converted string in a variable for future use.

## See Also
- [Official Python documentation on the lower() function](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Official Python documentation on the casefold() function](https://docs.python.org/3/library/stdtypes.html#str.casefold)