---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creating a Temporary File in Python: The Glippity-Glop and the Why

## What & Why?
Creating a temporary file means making a file that exists temporarily, usually just for a current session or task. Programmers do this to store temporary data without memory overhead or cluttering the actual file system. 

## How to:
In Python, we use the _tempfile_ module to create temporary files. Let's create a temp file and write some text to it.

```python
import tempfile

# Create a temporary file and write some data to it.
fp = tempfile.TemporaryFile()
fp.write(b'Stack Overflow!')
# Go back to the beginning and read data from the file.
fp.seek(0)
fp.read()
```
When you run this code, you'll get the output: 
```bash
b'Stack Overflow!'
```
This means you have successfully created a temporary file, written data to it, and read from it. Note that this temp file doesn't have a proper name; it's typically stored in your system's default location for temp files and is removed as soon as it's closed.

## Deep Dive
The concept of creating temporary files dates back to when disk space was relatively expensive. Programmers needed a way to temporarily store data without eating up valuable, pricey disk space. Today, while disk space is abundant and cheap, creating temporary files still helps in better memory management and keeping a clean file system.

An alternative to creating a temporary file is using in-memory data structures like lists or dictionaries, but these can lead to increased memory usage if not handled carefully. 

The _tempfile_ module in Python uses platform-specific methods to create temporary files in the most secure manner possible. Under the hood, when you create a temporary file, the module creates a unique file for you and takes care of deletion when the file is closed.

## See Also
To further help you in your Python journey, these resources might be useful:

- [Python’s official documentation on tempfile](https://docs.python.org/3/library/tempfile.html)
- [An article on Real Python about tempfile](https://realpython.com/python-tempfile/)
```
Take time to explore these and happy Pythoning!
```