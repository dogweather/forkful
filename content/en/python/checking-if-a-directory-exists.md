---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Checking if a Directory Exists in Python

## What & Why?

Checking if a directory exists involves verifying whether a specific file path points to an existing directory on your file system. Programmers do this to avoid errors from trying to access or write to nonexistent directories.

## How to:

Python's built-in `os` module provides simple ways to check if a directory exists:

```Python
import os

# This is your directory path
dir_path = "/path/to/your/directory"

# Use os.path.isdir() to check if it exists
if os.path.isdir(dir_path):
  print("The directory exists")
else:
  print("The directory doesn't exist")
```

This code will output either "The directory exists" or "The directory doesn't exist" depending on the actual state of the directory.

## Deep Dive

Historically, Python didn't have a direct method for checking the existence of a directory. Before the `os.path` module became popular, you'd have to try to open the directory and handle the exception if it didn't exist. Some people still use this try-except method as an alternative, although it's less readable.

Moreover, there's another alternative with `Path` from the `pathlib` module which is available from Python 3.4:

```Python
from pathlib import Path

dir_path = Path("/path/to/your/directory")

if dir_path.is_dir():
  print("The directory exists")
else:
  print("The directory doesn't exist")
```

Both `os.path` and `Path` access the file system, so there's little practical difference in terms of performance. The choice between them is mostly about readability, and `Path` can be more readable since it allows you to chain methods.

## See Also

To learn more about IO operations and file system in Python:

- [Python OS documentation](https://docs.python.org/3/library/os.html)
- [Python pathlib documentation](https://docs.python.org/3/library/pathlib.html)
- [Python For Beginners: File handling](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)