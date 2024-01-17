---
title:                "Checking if a directory exists"
html_title:           "Python recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

In Python, checking if a directory exists is the process of verifying if a specific file path corresponds to a directory on the machine. This is important for programmers because it allows them to handle different scenarios depending on whether the directory already exists or needs to be created.

## How to:

To check if a directory exists in Python, we can use the ```os.path.exists()``` function.
```python
import os

dir_path = "/path/to/directory"

if os.path.exists(dir_path):
  print("Directory exists")
else:
  print("Directory does not exist")
```
Sample output:
```
Directory exists
```

## Deep Dive

### Historical Context:

The ```os.path.exists()``` function was introduced in Python 2.4 as part of the ```os.path``` module. Before this, developers had to use the ```os.path.isfile()``` and ```os.path.isdir()``` functions separately to check if a file or directory exists respectively.

### Alternatives:

There are a few other ways to check if a directory exists in Python, such as using the ```os.access()``` function or the ```Path.exists()``` method from the ```pathlib``` module. However, the most commonly used and recommended method is still using ```os.path.exists()```.

### Implementation Details:

The ```os.path.exists()``` function takes in a path as an argument and returns a Boolean value ```True``` if the path exists and ```False``` if it does not. It can take both absolute and relative paths. It is worth noting that this function only checks for the existence of the path and does not differentiate between files and directories.

## See Also

- [Official documentation for ```os.path.exists()```](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [Geeks for Geeks article on checking file and directory existence in Python](https://www.geeksforgeeks.org/python-os-path-exists-method/)