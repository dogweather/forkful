---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is the task of accessing and interpreting the contents of a file as text, not binary. Programmers do it to obtain data, analyze text, and manipulate string content within a file.

## How to:

Let's start by reading a file line-by-line followed by the whole file at once.

For line-by-line:
```Python
with open('hello.txt', 'r') as file:
    for line in file:
        print(line, end='')
```

For the whole file at once:
```Python
with open('hello.txt', 'r') as file:
    content = file.read()
print(content)
```

Both snippets presume 'hello.txt' file is in the same directory and you've permission to read it.

The `with` keyword is used to ensure proper file handling.

## Deep Dive

Historically, dealing with files in Python was somewhat tedious. You had to manage file opening, reading, and closing manually. Problems like forgetting to close a file could lead to memory leaks. The `with` statement (introduced in Python 2.5, 2006) simplified this, ensuring the file is closed when the block inside `with` is exited.

Alternatives to `open()` function include the `fileinput` module, which is helpful when reading from multiple files.

Also, libraries like `pandas` provide powerful methods to read complex text files, especially great for data analysis. 

About implementation, when Python reads a file, it's not loading the entire thing into memory at once. It's reading and returning one line at a time when iterated using a for loop. On the other hand, `file.read()` reads the entire content at once and could exhaust memory for very large files.

## See Also

- [Python Official Docs: Reading and Writing Files](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python Official Docs: Fileinput](https://docs.python.org/3/library/fileinput.html)
- [Pandas Read_CSV](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html)