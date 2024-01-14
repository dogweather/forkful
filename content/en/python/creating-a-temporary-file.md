---
title:                "Python recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Temporary files are an essential part of programming in Python. They allow us to store temporary data or perform operations on files without creating permanent clutter on our systems. They are especially useful in situations where we need to create, modify, or process a large number of files quickly.

## How To
Creating a temporary file in Python is a simple process. We can use the `tempfile` module, specifically the `mkstemp()` function, to create a temporary file object.

```Python
import tempfile

# Create a temporary file object
temp_file = tempfile.mkstemp()

# Print the name of the temporary file
print(temp_file[1])

# Output: /var/folders/0j/kf7dj5322p7fsl_0bd8t81z00000gn/T/tmp582l6um0

# Write data to the temporary file
with open(temp_file[1], 'w') as f:
  f.write("This is a temporary file.")

# Read and print the contents of the temporary file
with open(temp_file[1], 'r') as f:
  print(f.read())

# Output: This is a temporary file.
```

## Deep Dive
When we use the `mkstemp()` function, we get a tuple containing two elements - a file descriptor and the absolute path to the temporary file. We can use the `fdopen()` function to convert the file descriptor to a file object and perform operations like reading and writing to the file.

Temporary files created using `mkstemp()` are not automatically deleted. We need to manually delete them using the `remove()` function from the `os` module or the `unlink()` function from the `pathlib` module.

```Python
import tempfile
import os
from pathlib import Path

# Create a temporary file object
temp_file = tempfile.mkstemp()

# Convert the file descriptor to a file object
file_object = os.fdopen(temp_file[0])

# Write data to the temporary file
file_object.write("This is a temporary file.")

# Delete the temporary file
os.remove(temp_file[1])

# Or using pathlib
Path(temp_file[1]).unlink()
```

## See Also
- [Python Docs - tempfile](https://docs.python.org/3/library/tempfile.html)
- [GeeksforGeeks - Temporary Files in Python](https://www.geeksforgeeks.org/temporary-files-python/)
- [RealPython - Working with Temporary Files in Python](https://realpython.com/working-with-temporary-files-in-python/)