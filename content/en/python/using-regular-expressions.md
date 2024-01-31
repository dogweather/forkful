---
title:                "Using regular expressions"
date:                  2024-01-19
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in strings. Programmers use regex for searching, editing, or manipulating text because it’s powerful and efficient.

## How to:
Below are Python examples using the `re` module for common regex operations:

```Python
import re

# Find all matches of 'abc' in a string
matches = re.findall('abc', 'abc123abc')
print(matches)  # Output: ['abc', 'abc']

# Search for 'def' and return a Match object
match = re.search('def', '123def456')
if match:
    print(match.group())  # Output: 'def'

# Replace 'ghi' with 'xyz'
replaced = re.sub('ghi', 'xyz', 'ghi123ghi')
print(replaced)  # Output: 'xyz123xyz'
```

## Deep Dive
Regular expressions have been around since the 1950s, developed alongside formal language theory. Alternatives to regex include parsing libraries and string methods like `str.find()` or `str.replace()`, but these lack the pattern-matching versatility of regex. Implementation-wise, Python uses the `re` module, which is based on the traditional UNIX regex library but includes some enhancements.

## See Also
- Python `re` module docs: https://docs.python.org/3/library/re.html
- Regular expression syntax guide: https://www.regular-expressions.info/
- Regex tester and debugger: https://regex101.com/
