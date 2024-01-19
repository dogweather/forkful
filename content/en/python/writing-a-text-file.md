---
title:                "Writing a text file"
html_title:           "Python recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Writing a Text File in Python: A No-Fuss Guide

Let's plunge right in.

## What & Why?

When we talk about writing a text file in Python, we're referring to the process of sending data to a file stored on your computer. This is useful for organizing and storing data for future use, like logging information, storing configurations or even data for a machine learning model.

## How to:

Below, we'll go through a simple example of writing a text file using Python's built-in `open()` function. 

```python
with open('test.txt', 'w') as file:
    file.write("Hello, World!")

```
The 'w' parameter here means we're opening the file in write mode. "Hello, World!" is the string we're writing into our 'test.txt' file.

If you open 'test.txt', it should show:
```
Hello, World!
```
## Deep Dive

Python's `open()` function comes from its early iterations, way back in the 90s, and has since been a simple, effective tool for dealing with files. While there are modules like `os` and `shutil` providing more file system functionality, `open()` handles the task of writing text files just fine.

Another way is using `os` module, but it's more low-level and complex for this task. The beauty about writing a text file this way in Python is simplicity. 

As for implementation details, `open()` returns a file object, and it's often used with two arguments: open(filename, mode). The second argument is optional; 'r' will be assumed if it’s omitted. Some commonly used modes are:
* 'r' for reading,
* 'w' for writing (an existing file with the same name will be erased),
* 'a' for appending,
* '+' opens the file for updating (reading and writing).

Note: Always be sure to close the file after use to free up system resources.

## See Also

Check out these resources for more in-depth look on working with files in Python:

1. [Python's Official Documentation on Reading and Writing Files](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
2. [Python File Handling: Create, Open, Append, Read, Write](https://www.guru99.com/reading-and-writing-files-in-python.html)
3. [Working With Files in Python – Real Python](https://realpython.com/working-with-files-in-python/)
4. [Python File I/O: Tutorialspoint](https://www.tutorialspoint.com/python/python_files_io.htm)