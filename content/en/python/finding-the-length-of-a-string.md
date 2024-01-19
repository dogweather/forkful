---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string involves determining the number of characters in it. Programmers do it to manipulate data, validate input, or loop through characters.

## How to:

Use Python's built-in `len()` function:

```Python
text = 'Hello, world!'
print(len(text)) # Outputs: 13
```

It's simple and works even with Unicode characters:

```Python
text = '你好，世界！'
print(len(text)) # Outputs: 6
```

## Deep Dive

Python's `len()` was already there in Python's earliest versions in the late '80s. It counts non-null-terminated characters, which is why you get the full Unicode character count.

An alternative: using a loop to manually count characters:

```Python
text = 'Hello, world!'
count = 0
for char in text:
  count += 1
print(count) # Outputs: 13
```

But `len()` is generally faster and more efficient unless you're doing something very peculiar indeed.

`len()` is an O(1) operation in Python. That's because Python strings are objects whose length is stored and accessible without needing to scan the entire string.

## See Also

Check out Python's official documentation on its built-in `len()` function: 
- [https://docs.python.org/3/library/functions.html#len](https://docs.python.org/3/library/functions.html#len)

And here's more on how Python implements its strings:
- [https://docs.python.org/3/c-api/unicode.html](https://docs.python.org/3/c-api/unicode.html)

There are plenty of additional Python string manipulations worth learning:
- [https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)