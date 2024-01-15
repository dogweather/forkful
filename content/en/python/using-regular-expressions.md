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

## Why

Regular expressions, or "regex" for short, are a powerful tool for pattern matching and text manipulation in Python. They allow you to search for specific patterns of characters within a larger string, making it easier to extract, validate, and manipulate data. 

## How To

To use regular expressions in Python, you first need to import the re module:

```Python
import re
```

Next, you can create a regular expression pattern using the `re.compile()` function and store it in a variable:

```Python
pattern = re.compile("hello")
```

Then, you can use this pattern with various methods like `match()`, `search()`, and `findall()` to search for specific patterns within a string. For example:

```Python
text = "Hello, world!"

# search for the pattern "hello"
result = pattern.search(text)

# print the matched pattern
print(result.group()) # output: "Hello"
```

You can also use special characters in your regular expressions to specify more complex patterns. For instance, the `.` character will match any single character, and the `+` character will match one or more instances of the preceding pattern. For example:

```Python
# search for the pattern "h.llo"
pattern = re.compile("h.llo")
text = "Hello, world!"

# find all matches
matches = pattern.findall(text)

# print the list of matches
print(matches) # output: ['Hello']
```

## Deep Dive

Regular expressions have a rich set of rules and symbols that allow you to create complex matching patterns. Here are a few commonly used ones:

- `.` - Matches any single character except for a new line
- `[]` - Matches any characters inside the brackets (e.g. `[abc]` will match a, b, or c)
- `[^]` - Matches any characters NOT inside the brackets (e.g. `[^abc]` will match any character except a, b, or c)
- `+` - Matches one or more instances of the preceding pattern
- `*` - Matches zero or more instances of the preceding pattern
- `?` - Matches zero or one instance of the preceding pattern
- `^` - Matches the start of a string
- `$` - Matches the end of a string
- `|` - Matches either the pattern on the left or the pattern on the right
- `()` - Groups multiple patterns together
- `\` - Escapes special characters

Regular expressions also have various flags that can modify their behavior, such as case-insensitivity and global matching. If you want to learn more about the different rules, symbols, and flags in regular expressions, there are numerous resources available online.

## See Also

- [Regular Expressions in Python](https://docs.python.org/3/library/re.html)
- [Python Regular Expression Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)
- [Regular Expressions 101](https://regex101.com/)