---
title:    "Python recipe: Concatenating strings"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Strings are an essential part of any Python program, and at some point, you might find yourself wanting to combine multiple strings into one. This is where concatenation comes in. By learning how to concatenate strings, you can manipulate and format your data in various ways to create more efficient and powerful programs.

## How To

Concatenating strings in Python is a simple task that can be achieved using the `+` operator. Let's take a look at an example:

```python
name = "John"
message = "Hello, " + name + ". Welcome to my blog!"
print(message)
```

Output: `Hello, John. Welcome to my blog!`

In the above example, we have defined two strings, `name` and `message`. The `+` operator is used to combine the two strings, with the variable `name` being added in between the other two strings. This creates a new string, which is then printed to the console.

Concatenation is not limited to just two strings, you can combine as many strings as you need. You can even concatenate strings with other data types, such as numbers.

```python
age = 32
sentence = "I am " + str(age) + " years old."
print(sentence)
```

Output: `I am 32 years old.`

Notice how we used the `str()` function to convert the integer value of `age` into a string, as the `+` operator can only be used to combine strings.

## Deep Dive

There are two other methods in Python that can be used for string concatenation - `str.join()` and `+=` operator. Let's take a look at these in detail.

The `str.join()` method takes a list of strings as its argument and returns a single string made by concatenating all the elements of the list.

```python
names = ["John", "Sarah", "Mike"]
message = ", ".join(names) + " are my friends."
print(message)
```

Output: `John, Sarah, Mike are my friends.`

Notice how we used the `", "` string to join the elements of the list, but you can use any other string or character.

The `+=` operator is similar to the `+` operator, but it also assigns the concatenated string back to the original variable.

```python
name = "John"
name += " Doe"
print(name)
```

Output: `John Doe`

## See Also

- [Python String Concatenation](https://www.w3schools.com/python/gloss_python_string_concatenation.asp)
- [Python String Operations](https://www.geeksforgeeks.org/python-string-operations/)
- [Official Python Documentation on Strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)