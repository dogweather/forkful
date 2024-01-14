---
title:                "Python recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to determine the length of a string in your Python code? Maybe you want to validate user input or limit the length of a text input. Whatever your reason may be, finding the length of a string is a common task in programming. In this blog post, we will explore how to do it efficiently and effectively.

## How To
To find the length of a string in Python, we can use the built-in `len()` function. This function takes in a string as an argument and returns the number of characters in the string. Let's take a look at an example:

```Python
string = "Hello World"
print(len(string))
```

The output of this code would be `11`, as there are 11 characters in the string "Hello World". Additionally, we can use this function on variables, not just on directly inputted strings. This allows us to find the length of dynamic strings.

```Python
name = input("Enter your name: ")
print("Your name is " + str(len(name)) + " characters long.")
```

In this example, we are using the `len()` function to determine the length of the user's inputted name and then printing it out in a sentence. This is just one of many ways that we can utilize the `len()` function to manipulate and display string lengths in our code.

## Deep Dive
Let's take a deeper look at the `len()` function and how it actually works behind the scenes. When we pass a string into the function, it goes through a process called "iteration". This means that the function goes through each character in the string and counts it, until it reaches the end of the string. This is why the function is able to determine the length of both static and dynamic strings, as it is counting the actual characters, not the variable names.

It is also worth noting that the `len()` function only counts the visible characters in a string. This means that if there are any invisible characters, like spaces or line breaks, they will not be counted in the length of the string. This can be useful when working with data and needing to exclude certain characters from the length count.

## See Also
- [Python Documentation on `len()` function](https://docs.python.org/3/library/functions.html#len)
- [W3Schools article on `len()` function](https://www.w3schools.com/python/ref_func_len.asp)
- [Real Python article on string manipulation in Python](https://realpython.com/python-strings/)

By now, you should have a good understanding of how to find the length of a string in Python using the `len()` function. Remember to always refer to reliable sources and practice writing your own code to solidify your understanding. Happy coding!