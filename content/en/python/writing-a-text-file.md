---
title:                "Python recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why Writing a Text File in Python is Useful

Writing a text file in Python is a useful skill to have because it allows you to store and manipulate data in a structured format. This can be especially beneficial for tasks such as data analysis or creating reports.

## How To Write a Text File in Python

To write a text file in Python, you can use the built-in `open()` function. This function takes two parameters: the file name and the mode in which the file will be opened. The mode is specified as a string, with "w" standing for write mode.

Here is an example code block in Python that creates a new text file called "sample.txt" and writes a sentence to it:

```python
# Create a file in write mode
file = open("sample.txt", "w")

# Write a sentence to the file
file.write("Hello there! This is a sample text file.")

# Close the file
file.close()

# Output: No output, but the file "sample.txt" has been created
```

To write multiple lines to a text file, you can use the `file.write()` function multiple times, or use the `file.writelines()` function, which takes in a list of strings as its parameter.

```python
# Create a file in write mode
file = open("sample.txt", "w")

# Write multiple lines to the file using file.writelines()
lines = ["First line\n", "Second line\n", "Third line"]
file.writelines(lines)

# Close the file
file.close()

# Output: No output, but the file "sample.txt" has been created with three lines of text
```

## Deep Dive into Writing Text Files in Python

When writing to a text file, you may encounter the issue of new lines being missing in the output. This is because the `file.write()` function does not automatically add a new line at the end of each string. To solve this, you can add a `\n` escape character at the end of each line.

You can also specify the encoding of the file when opening it with the `open()` function, which can be useful when working with special characters. For example, to write a file in UTF-8 encoding, you can use the following code:

```python
file = open("sample.txt", "w", encoding="utf-8")
```

Additionally, the `with` statement can be used to automatically close the file after writing, even if an exception occurs.

```python
with open("sample.txt", "w") as file:
    file.write("Hello there!")

# No need for file.close(), the file will automatically close after the block ends
```

## See Also
- [Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)
- [String Methods in Python](https://www.w3schools.com/python/python_strings.asp)
- [Unicode and Encodings in Python](https://realpython.com/python-encodings-guide/)

By learning how to write text files in Python, you can expand your programming skills and efficiently manage and manipulate data within your projects. Happy coding!