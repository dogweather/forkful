---
title:                "Python recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to change the case of a string in your Python code? Whether it's for formatting purposes or to compare user input, capitalizing a string can be a common task for many programmers. In this blog post, we'll explore how to easily capitalize a string in Python and dive deeper into the underlying concepts.

## How To

To capitalize a string in Python, we can simply use the `upper()` method, which will convert all characters in the string to uppercase. Let's take a look at an example:

```python
# Assign a string to a variable
my_string = "hello world"

# Use the upper() method and assign the result to a new variable
capitalized_string = my_string.upper()

# Print the result
print(capitalized_string)
```

The output will be `HELLO WORLD`. As you can see, the `upper()` method has converted all characters in the string to uppercase.

But what if we only want to capitalize the first letter of the string? We can use the `capitalize()` method, which will capitalize only the first character and convert the rest to lowercase. Let's see it in action:

```python
# Assign a string to a variable
my_string = "hello world"

# Use the capitalize() method and assign the result to a new variable
capitalized_string = my_string.capitalize()

# Print the result
print(capitalized_string)
```

The output will be `Hello world`, with the first letter capitalized and the rest in lowercase.

These methods are simple yet effective in capitalizing strings in Python. However, it's important to note that they only work with alphabetic characters. Special characters, numbers, and spaces will remain unchanged.

## Deep Dive

Now that we've seen how to capitalize strings in Python, let's dive deeper into the underlying concepts. The `upper()` and `capitalize()` methods are part of the `str` data type, which stands for "string". These methods are known as built-in methods, which means they are already available for use in Python without the need for importing any libraries.

It's also useful to know that strings are immutable in Python, which means they cannot be changed after they are created. This is why the `upper()` and `capitalize()` methods return a new string object instead of modifying the original one. This might seem counterintuitive at first, but it actually helps with data integrity and avoids unwanted side effects.

## See Also

- [Official Python Documentation on Strings](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Real Python Article on String Methods](https://realpython.com/python-strings/#string-methods)
- [GeeksforGeeks Tutorial on String Methods](https://www.geeksforgeeks.org/python-string-methods-set-1-find-replace-split-length-compare/)