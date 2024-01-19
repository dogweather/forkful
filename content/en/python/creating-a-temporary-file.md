---
title:                "Creating a temporary file"
html_title:           "Python recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file means to conjure up a file with content that aids in achieving an operation, but isn't needed in the long run. Programmers write to and read from these ephemeral storage mediums mainly for testing and transferring data between different processes. 

## How to:

Python's `tempfile` module contains functions that let you create and work with temporary files. Behold:

```Python
import tempfile

# Creating a temporary file
temp = tempfile.TemporaryFile()

# Writing content 
temp.write(b'Some data')

# Going back to the beginning and read data
temp.seek(0)
print(temp.read())

# Clean up the file
temp.close()
```

If you run the code, the output would be:

```Python
b'Some data'
```

## Deep Dive

Back in the day, programmers created temp files manually, often forgetting to delete them. Tempfiles in Python auto-delete when closed or on program exit, a nifty feature introduced in Python 2.0.

There are alternatives to using tempfiles. `StringIO` objects, for instance, can be used when your "file" fits well within memory and is only needed briefly. However, StringIO isn't suitable for interprocess communication or large data chunks. Another viable use case could be in-memory data structures (e.g., Lists or Dicts), but these aren't practical for data sharing either.

In Python, temporary files are created using the underlying OS functionalities. For instance, on Unix systems, it uses the C library function `mkstemp()`.

## See Also

- Python's official [`tempfile` docs](https://docs.python.org/3/library/tempfile.html)
- A deep-dive Stack Overflow [discussion](https://stackoverflow.com/questions/8577137/tempfile-vs-stringio) comparing `tempfile` and `StringIO`
- Python's official [`StringIO` docs](https://docs.python.org/3/library/io.html#io.StringIO)