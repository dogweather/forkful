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

## Why
Deleting characters matching a pattern is a common task in data processing and text editing. Whether you want to remove certain characters from a string or filter out unwanted data, knowing how to delete characters matching a pattern can save you time and effort in your coding endeavors.

## How To
To delete characters matching a pattern in Python, we can use the `re` module, which provides support for regular expressions. Regular expressions (or regex for short) are powerful tools for pattern matching and manipulation.

To get started, we first import the `re` module:

```Python
import re
```

Next, we define the string we want to manipulate:

```Python
string = "Hello! This is a sample string."
```

Let's say we want to delete all the digits from the string. We can use `re.sub()` to substitute the matching pattern with an empty string:

```Python
new_string = re.sub(r"\d+", "", string)

print(new_string)
```

Here, we're using the `\d+` pattern to match one or more digits in the string. The `r` before the pattern indicates that it should be treated as a raw string, avoiding any potential conflicting characters.

The output would be:

```
Hello! This is a sample string.
```

We can also use regex to remove certain characters. For example, if we want to remove all the vowels from the string, we can use:

```Python
new_string = re.sub(r"[aeiou]+", "", string, flags=re.IGNORECASE)

print(new_string)
```

The `[aeiou]+` pattern matches one or more vowels, and the `re.IGNORECASE` flag makes the matching case-insensitive. The result would be:

```
Hll! Ths s smpl strng.
```

## Deep Dive
The `re` module has various functions for pattern matching and manipulation, but `re.sub()` stands out for its usefulness in deleting characters matching a pattern. Its syntax is:

```
re.sub(pattern, repl, string, count=0, flags=0)
```

Where `pattern` is the regular expression pattern to match, `repl` is the replacement string, `string` is the input string, `count` is an optional parameter to specify the maximum number of replacements, and `flags` allows us to modify the behavior of the function.

It's important to note that `re.sub()` doesn't modify the original string, but rather returns a new string with the substitutions made. This is why we assign the result to a new variable in our examples.

To learn more about regular expressions and their patterns, check out the Python `re` module documentation and tutorials listed in the "See Also" section.

## See Also
- [Python `re` module documentation](https://docs.python.org/3/library/re.html)
- [Real Python - How to Use Regular Expressions in Python](https://realpython.com/regex-python/)
- [RegexOne - Interactive Regular Expressions Tutorial](https://regexone.com/)