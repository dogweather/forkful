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

Checking if a directory exists in Python involves confirming if a specified folder or directory is present in a file system. Programmers do this to avoid runtime exceptions while attempting to access nonexistent directories.

## How To:

You can check if a directory exists by using the `os.path` module in Python. Here's a simple example:

```Python
import os

# specify the directory name
directory_name = '/path/to/your/directory'

# check directory
if os.path.isdir(directory_name):
    print('Directory exists.')
else:
    print('Directory does not exist.')
```
If your directory exists, the output will be:

```'Directory exists.'```

Otherwise, you'd see:

```'Directory does not exist.'```

## Deep Dive:

The method above uses the Python built-in module `os.path`. This module has been part of Python since its initial releases in the late 1990s, so it's pretty reliable and universally accepted.

However, if you're using Python 3.4 and above, you also have the option of using the `pathlib` module. This module is object-oriented, meaning it represents filesystem paths as objects instead of plain strings. Here's how it works:

```Python
from pathlib import Path

# specify the directory name
directory_name = '/path/to/your/directory'

# check directory
if Path(directory_name).is_dir():
    print('Directory exists.')
else:
    print('Directory does not exist.')
```

This newer, object-oriented approach provides a more intuitive and pythonic way to interact with filesystem paths. However, for compatibility and simplicity, many folks still prefer the good old-fashioned `os.path` module.

## See Also:

Here are some additional resources you might find helpful:

1. [Python `os.path` official documentation](https://docs.python.org/3/library/os.path.html)
2. [Python `pathlib` official documentation](https://docs.python.org/3/library/pathlib.html)
3. [Dive Into Python's Chapter on Files](http://diveintopython3.problemsolving.io/files.html)
4. [Python's `os` module's useful filesystem methods](https://docs.python.org/3/library/os.html)