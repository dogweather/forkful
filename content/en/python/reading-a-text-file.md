---
title:    "Python recipe: Reading a text file"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how to read and process a text file using Python? Reading text files is a common task in many programming projects, and mastering this skill can make you a more efficient developer. In this blog post, we will explore the basics of reading a text file in Python and provide some useful tips to get you started.

## How To
To read a text file in Python, we can use the `open()` function and specify the name and path of the file we want to read. We also need to specify the mode in which we want to open the file, which in this case is "r" for read mode. Here's an example:

```Python
f = open("text_file.txt", "r")
```
Once the file is open, we can use various methods to read the contents of the file. The most common method is `read()`, which reads the entire file and returns its contents as a string. Here's an example:

```Python
f = open("text_file.txt", "r")
contents = f.read()
print(contents)
```
Output:
```text
This is a text file.
It contains some random text for our example.
We can read it using Python.
```
Another useful method is `readline()`, which reads one line at a time. Here's an example:

```Python
f = open("text_file.txt", "r")
line = f.readline()
print(line)
```
Output:
```text
This is a text file.
```
To read the entire file one line at a time, we can use a `for` loop and the `readlines()` method. Here's an example:

```Python
f = open("text_file.txt", "r")
for line in f.readlines():
    print(line)
```
Output:
```text
This is a text file.
It contains some random text for our example.
We can read it using Python.
```

## Deep Dive
When reading a text file, it's important to remember that the file pointer (i.e. the current position in the file) moves as we read it. This means that if we try to read the same file again, we won't get any output because the pointer is already at the end of the file. To reset the pointer, we can use the `seek()` method and specify the position where we want it to be. For example, to reset the pointer to the beginning of the file, we can use `f.seek(0)`.

Another important consideration when reading a text file is how to handle different encoding types. By default, the `open()` function uses the system's default encoding, which may not be suitable for all files. To specify a specific encoding type, we can use the `encoding` argument. For example, if our file is encoded in UTF-8, we can use `f = open("text_file.txt", encoding="utf-8")`.

Lastly, after we finish reading a file, it's important to close it using the `close()` method. This ensures that all resources used by the file are freed. Alternatively, we can use the `with` statement, which automatically closes the file after the indented code is executed. Here's an example:

```Python
with open("text_file.txt", "r") as f:
    contents = f.read()
    # do something with the contents

# file is automatically closed outside of the "with" statement
```

## See Also
- [Python documentation on file input/output](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python article on working with files](https://realpython.com/read-write-files-python/)
- [Tutorial on reading and writing CSV files in Python](https://www.geeksforgeeks.org/python-read-write-csv-files/)