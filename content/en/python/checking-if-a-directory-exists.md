---
title:    "Python recipe: Checking if a directory exists"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

In Python programming, it's important to have an efficient way of checking if a directory exists. This can save you time and effort in handling errors and ensure that your code runs smoothly.

## How To

To check if a directory exists in Python, we can use the `os.path` module. This module provides functions for working with file paths and directories.

First, let's import the `os.path` module:

```Python 
import os.path
```

Next, we can use the `isdir()` function to check if a directory exists. This function takes in a file path as its argument and returns `True` if the file path is a directory, and `False` if it is not.

```Python 
# Check if directory "images" exists 
print(os.path.isdir("images"))
```

Output:
```
True
```

We can also use the `exists()` function to check if a directory or file exists at a given path. This function returns `True` if the path exists, and `False` if it does not.

```Python 
# Check if directory or file exists at path
print(os.path.exists("images/logo.png"))
```

Output:
```
True
```

## Deep Dive

The `os.path` module also provides functions for manipulating file and directory paths, such as `join()` and `abspath()`. These can be useful when working with different operating systems or for ensuring that the correct path is being checked.

It's important to note that the `isdir()` and `exists()` functions only check if the path given to them exists at the time of execution. If the path is created or moved after the code has been run, the functions will still return the initial result.

## See Also

Here are some helpful resources for further learning:

- [Python Documentation on os.path module](https://docs.python.org/3/library/os.path.html)
- [RealPython article on manipulating file paths in Python](https://realpython.com/python-pathlib/)
- [Stack Overflow discussion on checking if a directory exists in Python](https://stackoverflow.com/questions/8933237/how-to-find-if-directory-exists-in-python)