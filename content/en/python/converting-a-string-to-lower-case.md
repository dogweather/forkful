---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case in Python simply means transforming all the uppercase letters in a string to lowercase. Programmers do this to ensure uniform data, avoid duplication, and enable case-insensitive comparisons.

## How To:

Here's the Python code:

```Python
text = "Hello, World!"
lowercase_text = text.lower()
print(lowercase_text)
```

This will output:

```Python
'hello, world!'
```

## Deep Dive:

In Python, the `.lower()` method has been around since Python 1.5. It's a built-in method that directly works on string data types. 

-An alternative to using the `.lower()` method is using the Python's `str.casefold()` method, which is a more aggressive method than `str.lower()`. It's used for caseless matching, meaning it not only converts uppercase characters to lowercase, but even does a good job with non-ASCII characters.

-As for implementation, Python's `str.lower()` method follows Unicode Standard Annex #30 for Unicode character conversions. It makes sure to respect the cultural differences in lowercase character representations.

## See Also:

- Python Official Documentation: https://docs.python.org/3/library/stdtypes.html#str.lower 
- Unicode Standard Annex #30: https://unicode.org/reports/tr30/
- Python’s casefold(): https://docs.python.org/3/library/stdtypes.html#str.casefold