---
title:                "Python recipe: Writing a text file"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 

Writing a text file is a fundamental skill in Python programming. It allows us to store data, such as user input or program output, in a simple and readable format. Text files are also commonly used for data manipulation and analysis, making it an essential skill for any Python programmer.

## How To 

To write a text file in Python, we can use the built-in `open()` function. This function takes two parameters: the file name we want to create or open, and the mode in which we want to open the file. For writing a new file, we need to specify the mode as `'w'`.

```python
# Creating a new text file named "example.txt"
file = open("example.txt", "w")

# Writing "Hello World!" to the file
file.write("Hello World!")

# Closing the file to save changes
file.close()
```

By default, the `write()` method will overwrite any existing file with the same name. If we want to add data to an existing text file without overwriting it, we can specify the mode as `'a'` (append). 

```python
# Opening an existing text file named "example.txt" in append mode
file = open("example.txt", "a")

# Adding a new line to the file
file.write("\nThis is a new line!")

# Closing the file to save changes
file.close()
```

To write multiple lines to a file, we can use the `write()` method multiple times or use the `writelines()` method, which takes a list of strings as input. Let's see this in action:

```python
# List of strings to write to the file
lines = ["This is line 1.\n", "This is line 2.\n", "This is line 3.\n"]

# Using write() method
file = open("example.txt", "w")
file.write("This is line 1.\n")
file.write("This is line 2.\n")
file.write("This is line 3.\n")
file.close()

# Using writelines() method
file = open("example.txt", "w")
file.writelines(lines)
file.close()
```

After running the code above, the content of our "example.txt" file will be:

```
This is line 1.
This is line 2.
This is line 3.
```

## Deep Dive 

When writing a text file, we should keep in mind the following points:

- If we do not specify the full path of the file, it will be created in the current working directory.
- It is best practice to use the `with` statement when opening a file. This ensures that the file is automatically closed after the code block is executed.
- To avoid any formatting issues, always add a newline `\n` at the end of each line when using the `write()` method.
- The `write()` method only takes in strings, so we need to convert any other data types to strings before writing it to the file.

## See Also 

- [Python documentation on file input/output](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python tutorial on working with files](https://realpython.com/read-write-files-python/)
- [Python Crash Course book on file manipulation](https://nostarch.com/pythoncrashcourse/)