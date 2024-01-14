---
title:                "Python recipe: Converting a string to lower case"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

As Python programmers, we often come across situations where we need to manipulate strings. One common task is converting a string to lower case. This can be useful for data cleaning, standardizing inputs, and more. In this blog post, we will explore why and how to convert a string to lower case in Python.

## How To

```Python
# Define a string
my_string = "Hello World"

# Convert the string to lower case
lower_string = my_string.lower()

# Print the output
print(lower_string)

# Output: hello world
```

In the above code, we first define a string and then use the `lower()` method to convert it to lower case. This method returns a new string with all characters converted to lower case. We then print the output to verify the conversion.

We can also apply this method to user input:

```Python
# Get user input
user_input = input("Enter a string: ")

# Convert the input to lower case
lower_input = user_input.lower()

# Print the output
print(lower_input)
```

## Deep Dive

The `lower()` method is a part of the string class in Python. It works by taking the original string and changing each uppercase character to its lowercase equivalent. It is important to note that this method does not modify the original string, but rather creates a new string with the converted characters.

One thing to keep in mind is that the `lower()` method will only convert uppercase characters to lowercase. Any other characters, such as numbers or special characters, will remain the same. For example, `123ABC` will become `123abc` after using the `lower()` method.

Another thing to note is that the `lower()` method is case sensitive. This means that "A" and "a" are considered different characters and will have different results after using the method.

## See Also

To learn more about string manipulation in Python, check out these resources:

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Official Python Documentation on Strings](https://docs.python.org/3/library/string.html)
- [String Manipulation with Python: Tips and Tricks](https://realpython.com/python-string-manipulation/)

Now that you know how to convert a string to lower case in Python, try applying this method in your own projects! Happy coding!