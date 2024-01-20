---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:58:04.104982-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Python is about confirming the presence of a folder on the file system before performing actions on it. Programmers do this to avoid errors like trying to access or write to a directory that isn't there.

## How to:

Python makes checking for a directory straightforward with the `os` and `pathlib` modules:

Using `os.path`:
```python
import os

# Check if the directory exists
if os.path.isdir("/path/to/directory"):
    print("The directory exists.")
else:
    print("The directory does not exist.")
```

Using `pathlib`:
```python
from pathlib import Path

# Check if the directory exists
directory = Path("/path/to/directory")
if directory.is_dir():
    print("The directory exists.")
else:
    print("The directory does not exist.")
```

Sample Output:
```
The directory exists.
```
or
```
The directory does not exist.
```

## Deep Dive:

Historically, Python used the `os` module for filesystem operations. However, `os.path.isdir()` was the de facto standard for checking directories. The problem was that `os.path` worked with strings for paths which could be clumsy.

Enter the more modern `pathlib` module, introduced in Python 3.4. It uses object-oriented paths, making code more readable and concise. Now you've got `Path.is_dir()`, a method that not just tidies up your code, but there's something pleasant about chaining method calls to a Path object.

If these methods return `False` for a non-existent directory, it could mean two things: either the directory genuinely isn't there, or your program lacks the permission to see it.

## See Also:

1. `os` module documentation: https://docs.python.org/3/library/os.html
2. `pathlib` module documentation: https://docs.python.org/3/library/pathlib.html
3. File system permissions in Python: https://docs.python.org/3/library/os.html#os.access