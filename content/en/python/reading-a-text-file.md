---
title:                "Reading a text file"
html_title:           "Python recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in Python refers to the process of opening and retrieving data stored as plain text within a .txt or similar file. Programmers do this because it's a common way to input, output, and store data.

## How to:

To read a text file in Python, you don't need anything fancy - just built-in Python functions will do. Let's have a look at an example:

```Python
file_path = 'your_file_path_here.txt'

# Using the built-in open function
with open(file_path, 'r') as file:
    print(file.read())
```

Just replace the `'your_file_path_here.txt'` with the path to your file, and the script will print the contents of the file into your console.

## Deep Dive

Python's built-in `open` function dates back to its earliest versions. From Python 2.5 onward, the preferred method has been to use the `with` keyword, leading to better cleanup of resources.

There are multiple ways to read a file such as readline(), readlines().  Or you can directly iterate over the file object. Here's an example:

```Python
with open(file_path, 'r') as file:
    for line in file:
        print(line)
```

This script prints one line of the file at a time. It's a more memory-friendly approach when you're dealing with a very large file and you only want to process one line at a time.

## See Also

For a deeper understanding of file input and output in Python and different methods to read a file, see these sources:

- Python's official docs on Input and Output: https://docs.python.org/3/tutorial/inputoutput.html
- Real Python's tutorial on reading and writing files: https://realpython.com/read-write-files-python/
- W3Schools tutorial on file handling: https://www.w3schools.com/python/python_file_handling.asp