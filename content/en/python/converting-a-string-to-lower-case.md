---
title:                "Python recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to convert a string to lower case in your Python code? Maybe you are working with user input or manipulating text data, either way, converting a string to lower case is a common task in Python programming. In this blog post, we will discuss why it is important and how to do it effectively.

## How To

To convert a string to lower case in Python, we can use the built-in function `lower()`. Let's take a look at an example:

```Python
string = "Hello, WORLD!"
print(string.lower())
```

The output of this code will be `hello, world!`, with all the letters in lower case. In this example, we first assign a string to a variable `string` and then use the `lower()` function to convert it to lower case. 

We can also use the `lower()` function on user input, for example:

```Python
name = input("Enter your name: ")
print("Nice to meet you, " + name.lower())
```

In this case, the user's input will be converted to lower case before being concatenated with the rest of the string. This can be useful when we want to compare strings or manipulate text data.

## Deep Dive

Strings in Python are immutable, which means they cannot be changed once they are created. So when we use the `lower()` function on a string, a new copy of the string with all lowercase letters is created. The original string remains unchanged. This can be a bit confusing at first, but it is important to remember when working with strings in Python.

The `lower()` function uses the ASCII (American Standard Code for Information Interchange) table to convert uppercase letters to lowercase. This means it only works on English characters and cannot convert special characters or letters from other languages to lower case. There is another function called `casefold()` which performs a more aggressive lowercasing, taking into account Unicode characters. However, for most cases, the `lower()` function is sufficient.

## See Also

Here are some links for further reading on working with strings in Python:

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [String Manipulation in Python](https://realpython.com/python-strings/)
- [Python String Formatting](https://www.digitalocean.com/community/tutorials/how-to-use-string-formatters-in-python-3)

Now you know why and how to convert a string to lower case in Python. Happy coding!