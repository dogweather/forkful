---
title:                "Python recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine two or more strings together in your Python code? Whether you're trying to create a dynamic message, build a URL, or simply display information, concatenating strings is a crucial skill to have in your programming arsenal.

## How To

Concatenation in Python is the process of combining two or more strings into a single string. There are a few ways to do this, but the most common method is by using the addition (+) operator.

```Python
# Basic string concatenation
string_1 = "Hello "
string_2 = "world!"
result = string_1 + string_2
print(result) # Output: Hello world!

# Combining variables and strings
name = "John"
message = "Welcome to our website, " + name + "!"
print(message) # Output: Welcome to our website, John!
```

You can also use the string formatting method, which allows for more flexibility and control over the final string. This is especially useful when you need to include variables or complex expressions within your string.

```Python
# String formatting
name = "Emily"
age = 27
greeting = "Hi, my name is {} and I am {} years old.".format(name, age)
print(greeting) # Output: Hi, my name is Emily and I am 27 years old.
```

Another method is using the string concatenation shortcut using the += operator. This allows you to concatenate strings in a single line of code and is often used in loops or when combining a large number of strings.

```Python
# Using the += operator
text = ""
for num in range(5):
    text += str(num)
print(text) # Output: 01234
```

## Deep Dive

It's important to understand that in Python, strings are immutable objects, meaning they cannot be changed once they are created. This is why each time you concatenate strings, a new string object is created.

You may also come across the term "string interpolation", which is often used to describe the process of combining variables and strings through string formatting. Understanding this terminology can help you better communicate and understand code written by others.

Additionally, when concatenating a large number of strings, it is more efficient to use the join() method rather than the + operator. This method creates a list of strings and then joins them together using a specified separator. This prevents the creation of unnecessary string objects.

## See Also

Need more information on string concatenation in Python? Check out these helpful resources:

- [Python String Concatenation](https://www.programiz.com/python-programming/string-concatenation)
- [Python String Formatting](https://www.learnpython.org/en/String_Formatting)
- [Understanding String Concatenation and Immutability in Python](https://realpython.com/python-strings/#string-mutation)
- [String Operations in Python](https://docs.python.org/3/library/string.html#string-methods)