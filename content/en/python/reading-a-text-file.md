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

Reading a text file in Python is essentially the process of accessing and extracting data from a plain text document. Programmers often do this to either analyze the data or perform further processing on it.

## How to:

To read a text file in Python, you can use the built-in `open()` function. Here is a simple example:

```
with open('example.txt', 'r') as f:
    data = f.read()
    print(data)
```

This code opens a file named `example.txt` in read mode and stores its contents in the `data` variable. The `with` statement ensures that the file is closed automatically after use. Finally, the `print()` function outputs the file's contents to the console.

If you want to read the file line by line, you can use the `readlines()` method instead of `read()`. Here is an example:

```
with open('example.txt', 'r') as f:
    for line in f.readlines():
        print(line.rstrip())
```

This code reads each line from the file and uses the `rstrip()` method to remove any trailing whitespace before printing it.

## Deep Dive:

Historically, reading text files was an essential part of programming as most data was stored in plain text documents. However, with the rise of more complex data storage formats like databases and XML, reading text files is now not as common.

In Python, there are also other ways to read text files, such as using the `csv` module or the `pandas` library. These alternatives offer additional features and functionality, making it easier to work with specific data formats.

When reading a text file in Python, you can also specify the encoding to use. By default, it will use the system's default encoding, but you can specify a different one if needed. The `open()` function also allows for different modes, such as read/write or append, depending on your specific needs.

## See Also:

To learn more about reading text files in Python, you can refer to the official documentation for the `open()` function at https://docs.python.org/3/library/functions.html#open. You can also check out other resources such as the Real Python article on "Reading and Writing Files in Python" at https://realpython.com/read-write-files-python/. Happy coding!