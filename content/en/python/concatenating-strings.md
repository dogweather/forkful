---
title:                "Concatenating strings"
html_title:           "Python recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings in Python is the process of combining two or more strings into one longer string. This is a common practice used by programmers for tasks such as building dynamic messages or formatting text in a specific way.

## How to:
To concatenate strings in Python, you can use the `+` operator or the `join()` method. The `+` operator allows you to add two strings together, while the `join()` method joins multiple strings with a specified separator.

```Python
# Concatenation using the + operator
str_1 = "Hello"
str_2 = "World"
concatenated_str = str_1 + str_2
print(concatenated_str) # Output: HelloWorld

# Concatenation using the join() method
str_list = ["Hello", "World"]
separator = " "
concatenated_str = separator.join(str_list)
print(concatenated_str) # Output: Hello World
```

## Deep Dive:
Before the introduction of the `+` operator and the `join()` method, concatenation in Python was done using the `''` operator. This involved manually typing out each part of the desired string and using the operator to combine them. Another popular alternative to concatenating strings is using string formatting methods like `format()` or `f-strings`.

When concatenating strings, it is important to keep in mind that strings are immutable in Python. This means that instead of directly modifying the original string, a new string object is created when concatenating. This can cause performance issues if done repeatedly on large strings.

## See Also:
- [Python String Concatenation: Everything You Need to Know](https://realpython.com/python-string-concatenation/)
- [10 Python String Methods That You Should Know](https://www.freecodecamp.org/news/the-python-string-format-method-a-guide-for-beginners/)