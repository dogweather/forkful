---
title:                "Converting a string to lower case"
html_title:           "Python recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Converting Strings to Lower Case in Python: Straight to the Point

## What & Why?

Moving all the letters of a string to lower case in Python is mostly about sanitization and standardization. This ensures the same letter in different cases is not seen as different during data processing.

## How to:

Below is an example of converting a string to lower case in Python:

```Python
text = "Converting to LOWER case in PYTHON!"
lower_text = text.lower()
print(lower_text)
```

When you run the code, you should get the following output:

```Python
'converting to lower case in python!'
```
Simple as that!

## Deep Dive

The `lower()` function in Python had been around even in Python 1, hinting at its significance in the language's intent on text processing. It's a method under Python's built-in string class, with no additional libraries required.

Python provides several other ways of text-casing, for instance, `capitalize()` (where only the first letter of the string is capitalized), `title()` (where the first letter of each word is capitalized), or `upper()` (which converts all the letters to uppercase). 

Diving into the implementation details, when you call `lower()` on a string, Python iteratively changes each upper case Unicode character to its lower case equivalent based on the Unicode case mappings. This function is locale-invariant, meaning it would produce the same result regardless of your system's current language settings.

## See Also

1. Python documentation on string methods: https://docs.python.org/3/library/stdtypes.html#str.lower

2. Unicode Case Mappings: https://www.unicode.org/Public/UNIDATA/UnicodeData.txt

3. More on Python's String class: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str