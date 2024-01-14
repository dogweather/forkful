---
title:    "Python recipe: Capitalizing a string"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Have you ever wanted to make your strings stand out? Or maybe just wanted to follow proper capitalization rules? Enter the world of capitalizing strings in Python. In just a few simple steps, you can take your strings from lowercase to uppercase and everything in between.

## How To

First, let's start with the most basic function for capitalization - `capitalize()`. This function will capitalize the first letter of a string while leaving the rest untouched.

```
Python
my_string = "hello world"
print(my_string.capitalize())
```

This code will output "Hello world" as the first letter of the string has been capitalized. 

Next up is the `upper()` function. This function will convert all letters in a string to uppercase.

```
Python
my_string = "hello world"
print(my_string.upper())
```

The output for this code would be "HELLO WORLD". 

Finally, let's take a look at the `title()` function. This function will capitalize the first letter of every word in a string.

```
Python
my_string = "hello world"
print(my_string.title())
```

The result for this code would be "Hello World". 

It's also important to note that these functions do not permanently change the original string, but rather return a new string with the desired capitalization. If you want to save the changes, you will need to assign the capitalized string to a variable.

## Deep Dive

Now that we have covered the basic functions for capitalization, let's dive a bit deeper. Did you know that there is also a function for swapping the case of a string? The `swapcase()` function will convert all lowercase letters to uppercase and vice versa.

```
Python
my_string = "hElLo WoRlD"
print(my_string.swapcase())
```

The output for this code would be "HeLlO wOrLd".

It's also worth noting that these functions work not only with standard letters, but also with special characters and numbers. So don't be afraid to get creative with your strings!

## See Also

To learn more about the functions and methods mentioned in this article, check out the official Python documentation:

- [capitalize()](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [upper()](https://docs.python.org/3/library/stdtypes.html#str.upper)
- [title()](https://docs.python.org/3/library/stdtypes.html#str.title)
- [swapcase()](https://docs.python.org/3/library/stdtypes.html#str.swapcase)