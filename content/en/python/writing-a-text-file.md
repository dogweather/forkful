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

## Why
Text files are a common way to store and transfer data in the programming world. Understanding how to write a text file in Python opens up possibilities for organizing and manipulating data within a program.

## How To
Writing a text file in Python is a simple process that can be accomplished in just a few lines of code. 

First, the `open()` function is used to create a file object, which is then assigned to a variable. The `open()` function takes two arguments: the name of the file to be created, and the mode in which it should be opened. The `w` mode indicates that the file will be opened for writing.

```Python
file = open("my_file.txt", "w")
```

Next, the `write()` method is called on the file object, with the text to be written as the argument. This method adds the specified text to the file.

```Python
file.write("This is my first text file.")
```

Finally, the `close()` method is used to close the file. This step is important as it frees up system resources that were used to manipulate the file.

```Python
file.close()
```

To ensure that the file has been successfully created and written to, we can use the `read()` method to read the contents of the file and print it out.

```Python
file = open("my_file.txt", "r")
print (file.read())
```

Running this code will produce the following output in the console:

```
This is my first text file.
```

## Deep Dive
There are a few different modes in which a file can be opened for writing in Python. The `w` mode, as used in the example above, will create a new file if one with the specified name doesn't already exist. If a file with the same name does exist, it will be overwritten with the new content.

If we wanted to append new text to an existing file, we would use the `a` mode instead. This will add the new text to the end of the existing file, rather than overwriting it.

```Python
file = open("my_file.txt", "a")
file.write(" This is an appended line of text.")
file.close()
```

Running the same `read()` and `print()` code as before will now produce the following output:

```
This is my first text file. This is an appended line of text.
```

It's also worth noting that the `open()` function can take an optional third argument to specify the encoding of the file. This is important when dealing with different character sets and languages.

## See Also
- Python documentation on [Reading and Writing Files](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Understanding File Handling in Python](https://realpython.com/read-write-files-python/)
- A tutorial on [Working with Text Files in Python](https://www.programiz.com/python-programming/file-operation)