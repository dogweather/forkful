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

## Why
Concatenating strings is a common task in Python and it allows you to combine different text elements together to create a larger string. This can be useful for formatting, data manipulation, or displaying information to users.

## How To
Concatenating strings in Python is a simple process. You can use the built-in "+" operator to combine two or more strings together, or you can use the "join()" method to join multiple strings with a delimiter.

```python
# Using the "+" operator
first_name = "John"
last_name = "Doe"
full_name = first_name + " " + last_name
print(full_name) 
# Output: John Doe

# Using the "join()" method
strings = ["Hello", "world", "!"]
result = " ".join(strings)
print(result)
# Output: Hello world!
```

You can also use formatted strings by adding "f" before the quotation marks and using curly braces {} to insert variables or expressions into the string.

```python
# Formatted strings
age = 25
message = f"I am {age} years old."
print(message)
# Output: I am 25 years old.
```

## Deep Dive
Strings in Python are immutable, meaning that they cannot be modified in-place. When you use the "+" operator or "join()" method to concatenate strings, a new string object is created and the original strings remain unchanged. This is why it is important to store the result of concatenation in a variable, rather than just printing it out.

In addition, you can also use the "format()" method to insert variables or expressions into a string, instead of using formatted strings. This method uses placeholder brackets {} that can be replaced with the provided values.

```python
# Using the "format()" method
first_name = "Jane"
last_name = "Doe"
full_name = "My name is {0} {1}.".format(first_name, last_name)
print(full_name)
# Output: My name is Jane Doe.
```

It is also worth noting that string concatenation in Python can become inefficient when dealing with a large number of strings. This is because each time a new string is created, the previous strings must be copied over to the new object, leading to more memory usage and slower performance. In such cases, it is recommended to use the "join()" method or the "format()" method instead.

## See Also
- Official Python Documentation on strings: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Real Python article on string concatenation: https://realpython.com/python-strings/#concatenating-strings
- W3Schools tutorial on string concatenation in Python: https://www.w3schools.com/python/python_strings_concatenate.asp