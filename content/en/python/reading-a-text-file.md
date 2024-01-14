---
title:                "Python recipe: Reading a text file"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Are you new to programming and wondering why you would want to learn how to read a text file? Well, text files are a common way of storing data, and being able to read them is an essential skill for any aspiring programmer. Whether you're working with large datasets or simply want to retrieve information from a file, knowing how to read a text file is a valuable skill to have in your coding arsenal.

## How To
To start, let's create a simple text file called "sample.txt" with the following contents:
```
Hello world
I love coding
```
In order to read this file, we will use a built-in function called `open()` that takes in the name of the file and the mode in which we want to open it (in this case, "r" for read mode). We will also use a `with` statement to ensure that the file is properly closed after we're done working with it.

```
with open("sample.txt", "r") as file:
    contents = file.read()

print(contents)
```

The output of this code would be:

```
Hello world
I love coding
```

We can also read the file line by line using a `for` loop:

```
with open("sample.txt", "r") as file:
    for line in file:
        print(line)
```

The output would be:

```
Hello world

I love coding
```

Lastly, if you want to only read a certain number of characters from the file, you can use the `read()` function and specify the number of characters you want to read:

```
with open("sample.txt", "r") as file:
    contents = file.read(5)

print(contents)
```

The output would be:

```
Hello
```

## Deep Dive
In addition to simple text files, there are many other types of files that can be read using similar methods, such as CSV, JSON, and XML files. The `open()` function also allows us to specify different modes for reading and writing to the file. You can also use other operations such as `seek()` to move to a specific position in the file and `tell()` to know the current position.

Reading a text file may seem like a simple task, but it is an important building block in more complex operations such as data manipulation and analysis. It is also important to handle errors that may occur while reading a file, such as the file not existing or not having the correct permissions.

## See Also
- [Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)
- [Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)
- [Python Documentation on `open()` function](https://docs.python.org/3/library/functions.html#open)