---
title:    "Python recipe: Concatenating strings"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Strings are an essential part of any programming language, allowing us to work with text data in our code. One common task when dealing with strings is to combine, or concatenate, them together. This allows us to create new strings from existing pieces of text and is useful in many situations. In this blog post, we will explore the reasons why we would want to concatenate strings and how to do it in Python.

## How To

Concatenating strings in Python is a straightforward process that can be done in a variety of ways. Let's take a look at some examples using different approaches:

```python
# Using the "+" operator
str1 = "Hello"
str2 = "World"
concatenated_str = str1 + str2
print(concatenated_str)
# Output: HelloWorld

# Using the "format" method
str1 = "Hello"
str2 = "World"
concatenated_str = "{} {}".format(str1, str2)
print(concatenated_str)
# Output: Hello World

# Using f-strings (Python 3.6+)
str1 = "Hello"
str2 = "World"
concatenated_str = f"{str1} {str2}"
print(concatenated_str)
# Output: Hello World
```

We can see that in all three examples, we are combining two strings together to create a new one. In the first example, we use the concatenation operator "+" to join the two strings. In the second and third examples, we use string formatting methods to insert the string variables into a template string. This allows for more flexibility in the final output string.

Concatenating strings can also involve more than just two strings. We can easily join multiple strings together using the same methods, as shown in the examples below:

```python
# Joining multiple strings
str1 = "Hello"
str2 = "beautiful"
str3 = "world"
concatenated_str = str1 + " " + str2 + " " + str3
print(concatenated_str)
# Output: Hello beautiful world

# Formatting with multiple variables
str1 = "I"
str2 = "am"
str3 = "hungry"
concatenated_str = "{} {} {}".format(str1, str2, str3)
print(concatenated_str)
# Output: I am hungry

# Using f-strings with multiple variables
str1 = "Welcome"
str2 = "to"
str3 = "my"
str4 = "blog"
concatenated_str = f"{str1} {str2} {str3} {str4}"
print(concatenated_str)
# Output: Welcome to my blog
```

As we can see, the possibilities are endless when it comes to concatenating strings in Python. It is a simple yet powerful tool that can be used in various scenarios.

## Deep Dive

Under the hood, concatenating strings involves creating a new string object with the joined contents of the individual string variables. In Python, strings are immutable, meaning they cannot be changed. So when we concatenate strings, we are actually creating a new object in memory. This can have implications in terms of performance when working with large strings or a large number of concatenations.

One way to optimize string concatenation in Python is to use the `join` method. This method takes a list of strings and joins them together using the specified delimiter. Here's an example:

```python
# Using the join method
str_list = ["Python", "is", "awesome"]
delimiter = " "
concatenated_str = delimiter.join(str_list)
print(concatenated_str)
# Output: Python is awesome
```

Using the `join` method can be more efficient than concatenating strings one by one, especially with larger inputs.

## See Also

For more information on string concatenation in Python, check out these resources:

- [Python String Concatenation](https://www.programiz.com/python-programming/string-concatenation)
- [Python String Formatting](https://realpython.com/python-string-formatting/)
- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)