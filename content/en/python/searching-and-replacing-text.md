---
title:                "Python recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Whether you are a seasoned programmer or just starting out, one of the most common tasks you will encounter is searching and replacing text. This can be for a variety of reasons, such as fixing typos, updating variable names, or making global changes to your code. Whatever the reason may be, learning how to efficiently search and replace text using Python can save you time and frustration.

## How To

The first step in searching and replacing text using Python is to import the built-in `re` module, which stands for "regular expressions". This module allows us to use powerful pattern matching techniques to find and manipulate text.

```Python
import re
```

Next, we need to define the text we want to search through and the pattern we want to find and replace. Let's say we have the following string:

```Python
text = "I love python and all its amazing features."
```

And we want to replace "python" with "Ruby". We can do this by using the `sub` function from the `re` module and passing in the pattern we want to replace, the replacement text, and the original string.

```Python
new_text = re.sub("python", "Ruby", text)
```

The `sub` function will replace all instances of "python" with "Ruby" and return the new string. We can then print out the new text to see the changes.

```Python
print(new_text)
# Output: I love Ruby and all its amazing features.
```

But what if we want to replace "python" with "Ruby" and also change the "a" in "amazing" to an "e"? We can use regular expressions to handle this as well.

```Python
new_text = re.sub("python|amazing", "Ruby", text)
```

In this case, we are using the `|` symbol to indicate "or", so the `sub` function will replace both "python" and "amazing" with "Ruby".

```Python
print(new_text)
# Output: I love Ruby and ell its mizing features.
```

Additionally, we can use the `re.IGNORECASE` flag to make the search case-insensitive.

```Python
new_text = re.sub("python|amazing", "Ruby", text, flags=re.IGNORECASE)
```

Now both "python" and "Python" will be replaced with "Ruby".

## Deep Dive

Regular expressions can be intimidating, but once you understand the basics, they can be a powerful tool for search and replace tasks. Some common patterns that you might find useful include:

- `.` - Matches any character except newline
- `*` - Matches zero or more occurrences
- `+` - Matches one or more occurrences
- `?` - Matches zero or one occurrence
- `[]` - Matches any character inside the brackets
- `()` - Groups multiple patterns together
- `\b` - Matches at the beginning or end of a word

For more information on regular expressions and the various options and patterns you can use, check out the official Python documentation or this helpful cheatsheet: https://www.debuggex.com/cheatsheet/regex/python.

## See Also

- Official Python Documentation on Regular Expressions: https://docs.python.org/3/library/re.html
- Learn Python the Hard Way - Exercise 6 on Regular Expressions: https://learnpythonthehardway.org/python3/ex6.html 
- Practical Regular Expressions eBook: https://www.openshift.com/blog/practical-regex-ebook or https://www.sitepoint.com/advanced-regular-expressions/