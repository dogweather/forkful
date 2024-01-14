---
title:                "Python recipe: Checking if a directory exists"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why Checking if a Directory Exists in Python Is Useful

As a programmer, organizing files and directories is an essential task. Sometimes, it is necessary to check if a directory exists before performing operations such as creating or deleting files. This ensures that the program runs smoothly and prevents any errors from occurring.

## How To Check if a Directory Exists in Python

To check if a directory exists in Python, we can use the `os.path.exists()` function. This function takes the path of the directory as an argument and returns `True` if the directory exists and `False` if it does not. Here is an example:

```Python
import os

# Define the path of the directory
directory = "test_directory"

# Check if the directory exists
if os.path.exists(directory):
    print("The directory exists.")
else:
    print("The directory does not exist.")
```

Output:

```
The directory exists.
```

If the directory does not exist, the output will be:

```
The directory does not exist.
```

## Deep Dive into Checking if a Directory Exists in Python

Behind the scenes, the `os.path.exists()` function uses the `os.stat()` function to retrieve information on the given path. If the path exists, a `stat_result` object is returned, and the `st_mode` attribute is checked to determine if the path is a directory. If it is, the function returns `True`, and if it is not, it returns `False`.

Additionally, we can use the `os.path.isdir()` function to specifically check if the given path is a directory, which returns `True` or `False` without checking if the path exists.

```Python
import os

# Define the path of the directory
directory = "test_directory"

# Check if the path is a directory
if os.path.isdir(directory):
    print("The path is a directory.")
else:
    print("The path is not a directory.")
```

Output:

```
The path is a directory.
```

If the path is not a directory, the output will be:

```
The path is not a directory.
```

## See Also

- [os.path.exists() documentation](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [os.path.isdir() documentation](https://docs.python.org/3/library/os.path.html#os.path.isdir)
- [os.stat() documentation](https://docs.python.org/3/library/os.html#os.stat)