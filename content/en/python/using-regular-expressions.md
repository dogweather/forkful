---
title:                "Using regular expressions"
html_title:           "Python recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Getting to Grips with Python Regular Expressions

## What & Why?

Regular expressions (regex) are sequences of characters that form search patterns, used largely for "find" or "find and replace" functions in textual data. Programmers whip them out to sift through data quickly and efficiently, like a digital truffle pig.

## How to

Here's a basic example of how to use regular expressions in Python:

```python
import re

string = 'Hello, my name is AI and my serial number is 123456'
pattern = '\\bis\\b'

match = re.search(pattern, string)

if match:
  print("Match found")
else:
  print("No match")
```

If you run this, you'll see `Match found` because `is` exists in `string`.

## Deep Dive

Now let's pull back the curtains a bit:

Historically, regex comes from UNIX world, gaining popularity for its use in text processing tasks. While Python's built-in regex module (`re`) packs enough punch for most tasks, there are third-party libraries, like `regex`, offering more sophistication at the cost of complexity.

Key python.string methods such as `str.find()` or `str.replace()` can perform basic regex-like operations. For heavy-duty tasks though, `re` is the go-to choice. 

Under the hood, when `re.search()` or `re.match()` is called, Python compiles the regular expression to a series of bytecodes which can be executed by a matching engine written in C. Fascinating stuff.

Few things to keep in mind with Python regex: (1) Python’s raw string notation (`r"text"`) is often used as regex patterns, it saves you from battling escape sequences like double backslashes; (2) Remember methods like `re.findall()` to return all non-overlapping matches of pattern in string, as a list; (3) Don’t forget `re.split()`, a handy way to split your string using a regex pattern.

## See Also

Looking to go waist-deep into Python regex? Check out:
- [Python's official Regular Expression HOWTO](https://docs.python.org/3/howto/regex.html),
- [Python's `re` module](https://docs.python.org/3/library/re.html) documentation.
- [PyMOTW’s introduction to the `re` module](https://pymotw.com/3/re/)
- Joe Marini’s [course on LinkedIn learning](https://www.linkedin.com/learning/learning-regular-expressions) —a thorough walkthrough to mastering regex.

As you climb the programming ranks, mastering regex is a vital feather in your cap. So, get crackin'.