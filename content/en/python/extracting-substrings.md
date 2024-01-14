---
title:    "Python recipe: Extracting substrings"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Python program? Whether it's a word, a phrase, or even just a few characters, extracting substrings can be extremely useful in a variety of situations. Whether you're working with text data or processing input from users, knowing how to extract substrings can make your code more efficient and effective.

## How To

To get started with extracting substrings in Python, we first need to understand the basic syntax. We use square brackets to specify the indices of the substring we want to extract, with the first index being the starting position and the second index being the ending position. For example, if we have a string called "Hello World", and we want to extract the word "World", we would use the following code:

```python
s = "Hello World"
substring = s[6:11]
print(substring)
```

The output of this code would be "World", as expected. But what if we want to extract a substring from the beginning or end of our string? Luckily, Python provides a convenient alternative syntax for these cases. We can use a colon (:) on its own to specify that we want to extract all characters up to the specified index, or from the specified index to the end of the string. Let's see this in action:

```python
s = "Hello World"
substring1 = s[:5] #extracts "Hello" - up to index 5 (not including 5)
substring2 = s[6:] #extracts "World" - from index 6 to the end
print(substring1)
print(substring2)
```

The output of this code would be "Hello" and "World" respectively. This can be particularly useful when working with strings of unknown length or when we need to manipulate different parts of a string separately.

## Deep Dive

There are also some advanced techniques we can use for extracting substrings in Python. For example, we can use negative indices to start counting from the end of the string instead of the beginning. So, if we want to extract the word "World" from our earlier example, we could do so using negative indices like this:

```python
s = "Hello World"
substring = s[-5:]
print(substring)
```

Another useful technique is using the step parameter, which allows us to specify the distance between the characters we want to extract. For example, if we want to extract every other character from the string "Hello World", we could do so using a step of 2 like this:

```python
s = "Hello World"
substring = s[::2]
print(substring)
```

The output of this code would be "HloWrd", as it takes every second character from the string.

## See Also

- [Official Python documentation on strings and indexing](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Real Python article on slicing strings](https://realpython.com/python-string-slicing/)
- [GeeksforGeeks tutorial on substring extraction in Python](https://www.geeksforgeeks.org/python-string-slicing/)