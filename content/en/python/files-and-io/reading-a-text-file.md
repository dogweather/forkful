---
date: 2024-01-20 17:54:55.075228-07:00
description: "Reading a text file in Python is grabbing data from a file that's accessible\
  \ on your disk or over a network. Programmers read files to use the stored data\u2026"
lastmod: '2024-04-01'
model: gpt-4-1106-preview
summary: Reading a text file in Python is grabbing data from a file that's accessible
  on your disk or over a network.
title: Reading a text file
weight: 22
---

## What & Why?
Reading a text file in Python is grabbing data from a file that's accessible on your disk or over a network. Programmers read files to use the stored data (like configurations, user input, logs, etc.) within their applications.

## How to:
```python
# Reading the whole file at once
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)

# Reading line by line
with open('example.txt', 'r') as file:
    for line in file:
        print(line.strip())
```

## Deep Dive
Reading text files is fundamental - and has been around since the early days of programming. Python's straightforward `open` function has roots in the C standard library function `fopen`. Some alternatives for reading text files include using libraries such as `pandas` for CSVs or `json` for JSON files. Internally, when you read a file, Python asks the operating system to open a file stream, which is like a conveyor belt delivering data from the file to your program.

For large files, instead of `read()` which loads everything into memory, use `readline()` or iterate over the file object with a `for` loop to handle one line at a time – efficient and memory-friendly. While `with open` is the modern approach that automatically closes files, older scripts may use `file.close()` to do this manually, though it's error-prone if exceptions happen before the close call.

## See Also
- Python Documentation on IO: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Real Python Tutorial on Files: https://realpython.com/read-write-files-python/
- Python Official Docs for `open`: https://docs.python.org/3/library/functions.html#open
