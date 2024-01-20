---
title:                "Capitalizing a string"
html_title:           "Python recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Cool Caps: The Easy Way to Capitalize Strings in Python

## What & Why?

Capitalizing a string means turning the first letter of that string into an uppercase letter. We do this to format text data neatly, for instance when dealing with user input that should appear as a proper noun or a start of sentence.

## How to: 

Python’s standard library has built-in methods to make this easy-peasy. Let's have a look: 

```Python
# The capitalize() method:
text = "hello world"
cap_text = text.capitalize()
print(cap_text)

# Outputs: 'Hello world'
```

The `.capitalize()` method converts the first character of a string to uppercase letter and makes all other letters lowercase.

```Python
# The title() method:
text = "hello world"
title_text = text.title()
print(title_text)

# Outputs: 'Hello World'
```

The `.title()` method turns the first character of each word to uppercase and the rest to lowercase.

## Deep Dive

The capitalize and title methods have been part of Python's standard library since its early versions. They're efficient as they carry out the string transformation in one sweep.

An alternative is to use Python's string formatting with the `str.format()` or f-string methods. Here you capitalize individual letters manually, which is more work and less efficient:

```Python
text = "hello world"
format_text = "{}{}".format(text[0].upper(), text[1:].lower())
print(format_text)

# Outputs: 'Hello world'
```

Add the condition that the length of the string is more than 0 to avoid index errors with empty strings though.

These methods are implemented at a low level in C in the Python source code, which gives them a speed advantage over Python-level operations.

## See Also

For more on Python's string methods, check out the official Python docs on text sequence type str ([here](https://docs.python.org/3/library/stdtypes.html#textseq)).

For more advanced text processing, the PyPI's `textblob` package is very handy ([here](https://textblob.readthedocs.io/en/dev/)).

Remember, no small letter is safe. Keep on capitalizing!