---
title:    "Python recipe: Converting a string to lower case"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

In the world of programming, there are countless tasks that require string manipulation. One of these tasks is converting a string to lower case. Whether you are working with user input, database queries, or simply formatting text, having the ability to convert a string to lower case can be incredibly useful. It allows for consistency in data and makes it easier to search and compare strings. In this blog post, we will dive into how to convert a string to lower case in Python.

## How To

To convert a string to lower case in Python, we will use the built-in `.lower()` method. This method takes the string and returns a new string with all characters converted to lower case. Let's see an example:

```
Python
my_string = "HeLLo WoRLd"
print(my_string.lower())
```

The output of this code will be: `hello world`

We can also use the `.lower()` method on strings that contain special characters, numbers, and symbols:

```
Python
my_string = "!#PYTHON123"
print(my_string.lower())
```

The output of this code will be: `!#python123`

But what if we want to convert a string to lower case without creating a new string? This is where the `.lower()` method becomes even more useful. By using the `.lower()` method on a variable, we can permanently change the value of that variable to lower case:

```
Python
my_string = "UPPERCASE"
my_string = my_string.lower()
print(my_string)
```

The output of this code will be: `uppercase`

## Deep Dive

Now, let's take a deeper look at how the `.lower()` method converts a string to lower case. Under the hood, Python uses the Unicode standard to represent characters. Each character has a corresponding numerical code, known as a code point. In Python, the `.lower()` method uses the Unicode code point for each character to convert it to its lower case equivalent. This allows for consistency across different languages and character sets.

It's worth noting that the `.lower()` method only converts characters that have a corresponding lower case equivalent. For example, the German letter "ß" does not have a lower case equivalent and will remain unchanged when using the `.lower()` method.

## See Also

To learn more about string manipulation in Python, check out these resources:

- [Python documentation on the `.lower()` method](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Real Python's tutorial on string manipulation](https://realpython.com/python-strings/)
- [GeeksforGeeks article on using string methods in Python](https://www.geeksforgeeks.org/python-string-methods-set-1-find-replace-split-reverse-join/)