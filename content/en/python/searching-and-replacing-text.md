---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is all about finding a specific sequence of characters in a string, and then modifying it. Programmers often need this for tasks like data scrubbing, file renaming, pattern replacing, and refactoring code.

## How to:

Here's a classic Python 'replace' method:

```Python
text = "Hello, World!"
print(text.replace("World", "Python"))
```

The output will be:

```Python
Hello, Python!
```

In Python's regex (re) module, we find more advanced search and replace:

```Python
import re
text = "Hello, World!"
print(re.replace("World", "Python", text))
```

Again, our output:

```Python
Hello, Python!
```

## Deep Dive

Historically, the Unix 'sed' tool was the gold standard for text search and replace. Python has borrowed from its functionality, implementing it with simplicity and elegance.

Python's `replace` method is easy and fast for basic needs. When you need advanced functionality (e.g., case insensitive replace, limit the number of replacements), Python's `re` module comes into play. Under the hood, it uses complex algorithms to handle even multi-line strings efficiently.

## See Also

Python's str().replace() method: https://docs.python.org/3/library/stdtypes.html#str.replace

Python's re.sub() method: https://docs.python.org/3/library/re.html#re.sub

Comparison between Python's string methods and regex: https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285