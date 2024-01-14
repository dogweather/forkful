---
title:    "Python recipe: Extracting substrings"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

One common task in Python programming is extracting substrings from a larger string. This can be useful for tasks such as data cleaning and text analysis. 

## How To

To extract a substring in Python, we use the `substring[start:end]` notation. Here's an example:

```Python
text = "Hello World"
substring = text[0:5]
print(substring)
```

The output of this code would be `Hello`, as it extracts the characters from index 0 to index 5 of the original string. 

We can also use negative indexing to extract substrings. For example, `substring = text[-3:]` would extract the last three characters of the `text` string, resulting in `rld`.

In addition, we can use the `substring[start:end:step]` notation to extract a substring with a specified step size. For example, `substring = text[::2]` would extract every other character from the `text` string, resulting in `HloWrd`.

## Deep Dive

In Python, strings are immutable, meaning they cannot be changed. However, when we extract a substring, we are creating a new string that is a copy of the original string. This can be useful for manipulating text without altering the original string.

We can also use string methods such as `upper()` and `lower()` on extracted substrings to change the case of the characters. For example, `substring = text[0:5].upper()` would result in the substring being in all uppercase letters.

It's important to note that when extracting substrings, the `end` index is not inclusive. This means that if we want a substring to include the last character of a string, we need to use an index that is one higher than the position of the desired character. For example, `text[0:5]` includes characters at index 0, 1, 2, 3, and 4, but not 5.

## See Also

- [Tutorial: Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Documentation: Python Built-in Types](https://docs.python.org/3/library/stdtypes.html#string-methods)

By mastering the technique of extracting substrings in Python, you can enhance your skills in data manipulation and text analysis. Keep practicing and experimenting with different string methods to become a Python substring pro!