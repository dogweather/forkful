---
title:                "Python recipe: Finding the length of a string"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your Python code? Whether for data processing, input validation, or other tasks, being able to find the length of a string is a useful skill to have in your programming arsenal.

## How To

To find the length of a string in Python, you can use the built-in `len()` function. Here's an example:

```Python
my_string = "Hello world!"
length = len(my_string)
print(length)
```
Output:
```
12
```

In the above code, we first assign a string to the variable `my_string`. Then, using `len()`, we find the length of the string and assign it to the variable `length`. Finally, we print the value of `length` to see the output.

You can also use `len()` on other types of data, such as lists and tuples. Let's take a look at another example:

```Python
my_list = [1, 2, 3, 4, 5]
length = len(my_list)
print(length)
```
Output:
```
5
```

In this code snippet, we assign a list of numbers to the variable `my_list` and use `len()` to find its length. As you can see, the output is the number of elements in the list.

## Deep Dive

Behind the scenes, the `len()` function works by counting the number of items in an iterable object. An iterable object is a collection of items that can be iterated over, such as a string, list, or tuple.

But what if you want to find the length of a string without using `len()`? In that case, you can use a `for` loop to iterate through the string and count each character. Here's an example:

```Python
my_string = "Hello world!"
count = 0
for char in my_string:
  count += 1
print(count)
```
Output:
```
12
```

By using a `for` loop, we are able to iterate through each character in the string and increment the count by 1 for each character, ultimately finding the length of the string.

## See Also

For more information on finding the length of a string in Python, check out these helpful resources:

- [Python Documentation: Built-in Functions](https://docs.python.org/3/library/functions.html#len)
- [Real Python: Finding the Length of a String](https://realpython.com/python-len/)
- [Programiz: Python String length() Method](https://www.programiz.com/python-programming/methods/string/length)