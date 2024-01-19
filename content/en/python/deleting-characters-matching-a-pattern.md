---
title:                "Deleting characters matching a pattern"
html_title:           "Python recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Delete Characters Matching a Pattern in Python

## What & Why?

Deleting characters matching a specific pattern refers to the process of removing any instances of patterned sequences in strings. Programmers frequently do this when elements within the data cause confusion or malfunction in code, or when a data set should be reduced for easier reading and utilization.

## How to:

To teach this, we'll use Python's built-in `re` module, specifically the `re.sub()` function.

```Python
import re

# Example string
text = "B1C2D3F4A5"

# Define the pattern to delete - say we remove all numerical characters
pattern = "[0-9]"

# Use re.sub()
replaced_text = re.sub(pattern, "", text)
print(replaced_text)
```
The above code will output:

```Python
BCDFA
```

## Deep Dive

Historically, data cleaning and manipulation has been indispensable in programming and data analysis. With the `re` module's inception (module supporting RegEx operations), Python made string manipulation significantly easier.

As for alternatives, there's the string method `replace()`. However, it's less powerful as it doesn't support RegEx, meaning it can only remove exact, hard-coded strings.

In terms of implementation, `re.sub(pattern, repl, string, count=0)` works by substituting occurrences of the pattern in the string with `repl`. The optional `count` argument specifies the max number of pattern occurrences to replace.

## See Also

1. Python `re` module documentation: https://docs.python.org/3/library/re.html
2. More deep insights on RegEx: https://www.regular-expressions.info/
3. Python `str.replace()` method: https://docs.python.org/3/library/stdtypes.html#str.replace