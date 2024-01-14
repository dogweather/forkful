---
title:    "Python recipe: Converting a string to lower case"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
One of the most common tasks in programming is manipulating strings. Oftentimes, we need to convert strings to different formats to suit our needs. One useful conversion is changing a string to lowercase. This can be beneficial for things like data cleaning or string matching.

## How To
To convert a string to lowercase in Python, we can use the built-in `lower()` function. Let's take a look at a simple example:

```Python
string = "PYTHON PROGRAMMING"
lower_string = string.lower()

print(lower_string)
```

Output:
```
python programming
```

In the code above, we first define a variable `string` with a string value. Then, we use the `lower()` function on that string and assign it to a new variable `lower_string`. Finally, we print out the result, which is the lowercase version of our original string.

This method also works on strings with special characters or numbers. Let's see another example:

```Python
string = "Hello, 123!"
lower_string = string.lower()

print(lower_string)
```

Output:
```
hello, 123!
```

## Deep Dive
The `lower()` function works by looping through each character of the string and converting it to lowercase using the Unicode specifications. This means it can handle different languages and characters, making it a reliable choice for string conversion.

It's worth noting that the `lower()` function returns a new string rather than modifying the original string in place. This is important to keep in mind when working with mutable strings.

Another thing to consider is that the `lower()` function does not work on non-string data types. If you try to use it on an integer or float, for example, you'll get an error. It's always a good idea to check the data type before applying any string methods.

## See Also
- [Official Python Documentation for `lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [GeeksforGeeks: Python | String lower()](https://www.geeksforgeeks.org/python-string-lower/#:~:text=Python%20string_lower%20function%20returns,Python%20String%20lower%20example)
- [Real Python: String Formatting in Python](https://realpython.com/python-string-formatting/)