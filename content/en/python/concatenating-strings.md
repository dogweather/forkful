---
title:                "Python recipe: Concatenating strings"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 

As a programmer, you may come across situations where you need to join or combine two or more strings together. This process is known as string concatenation and it is a fundamental concept in any programming language, including Python. Understanding how to concatenate strings can greatly improve your ability to manipulate and organize data in your code. 

## How To 

In Python, you can concatenate strings using the plus sign (`+`) or the `format()` method. Let's take a look at some examples to see how this works:

```Python
# Concatenating strings using the plus sign
print("Hello" + "World")
# Output: HelloWorld

# Concatenating strings using the `format()` method
print("The {} brown {}".format("quick", "fox"))
# Output: The quick brown fox
```

In the first example, we use the plus sign to join two strings ("Hello" and "World") together. In the second example, we use the `format()` method to insert the words "quick" and "fox" into the string at specific positions, denoted by the curly brackets. 

You can also use the `+` and `format()` methods to concatenate more than two strings together. Keep in mind that the `format()` method is more versatile and allows you to insert variables and other data types into your strings. 

## Deep Dive 

In Python, strings are immutable, meaning they cannot be changed in place. This means that a new string object is created every time you perform a concatenation operation. To avoid creating multiple string objects, it is more efficient to use the `join()` method. This method takes in a list of strings and joins them together using a delimiter of your choice. 

You can also use the `join()` method in conjunction with list comprehensions to concatenate multiple strings. Let's see an example:

```Python
# Using the `join()` method
surnames = ["Smith", "Johnson", "Williams"]
full_names = ", ".join(surnames)
print(full_names)
# Output: Smith, Johnson, Williams

# Concatenating using list comprehensions and the `join()` method
greetings = ["Hello", "Hola", "Bonjour"]
full_greetings = " / ".join([greeting.upper() for greeting in greetings])
print(full_greetings)
# Output: HELLO / HOLA / BONJOUR
```

By using the `join()` method and list comprehensions, we can efficiently concatenate strings without creating multiple string objects. 

## See Also 
- [Python Documentation: String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Real Python: Python String Concatenation](https://realpython.com/python-string-concatenation/)
- [GeeksforGeeks: String Concatenation in Python](https://www.geeksforgeeks.org/python-string-concatenation/)

Using these resources, you can continue to deepen your understanding of string concatenation in Python and explore other methods and techniques. Happy coding!