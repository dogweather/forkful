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

## Why

If you regularly work with files and directories in your Python projects, you may need to check if a directory exists before performing any operations on it. This is to avoid errors and ensure the smooth execution of your code.

## How To
Checking if a directory exists in Python is a simple task that can be accomplished using the `os` module.

First, import the `os` module in your code:
```python
import os
```
Next, use the `path` module from the `os` module to check if a directory exists. The `path.exists()` method takes in the path of the directory as its argument and returns a boolean value `True` if the directory exists, or `False` if it does not.
```python
if os.path.exists("/path/to/directory"):
    print("Directory exists!")
else:
    print("Directory does not exist!")
```

You can also use the `os.path.isdir()` method to check if a given path is a directory or not. This method also returns a boolean value `True` if the path is a directory, or `False` if it is not.
```python
if os.path.isdir("/path/to/directory"):
    print("Path is a directory!")
else:
    print("Path is not a directory!")
```

### Sample Output

```python
>>> import os
>>> os.path.exists("/path/to/directory")
True
>>> os.path.isdir("/path/to/file")
False
```

## Deep Dive

When checking if a directory exists, it is important to note that the path can be either absolute (full path) or relative (path relative to the current working directory). 

One common mistake is assuming that specifying a path with a trailing slash indicates that it is a directory. This is not always the case. For example, the following code would return `False` even though the given path leads to a directory.
```python
os.path.isdir("/path/to/directory/")
```

Another scenario to be aware of is when working with user input. If the user inputs a path, it is important to perform additional checks to ensure that it is a valid path and not a malicious command.

## See Also

For further information on the `os` module and working with files and directories in Python, check out these resources:

- [Python Documentation on the `os` module](https://docs.python.org/3/library/os.html)
- [Real Python Tutorial on Working with Files and Directories in Python](https://realpython.com/working-with-files-in-python/)
- [Python File and Directory Operations on GeeksforGeeks](https://www.geeksforgeeks.org/python-files-directory-operations/)