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

##
## What & Why?

Writing a text file is the process of creating a new file on your computer and filling it with text data. Programmers often use this method to store and organize important information that can be easily accessed and modified later on.

## How to:

```Python
# Creating/opening a text file
myfile = open("sample.txt", "w")

# Write to the file
myfile.write("Hello Python!")

# Close the file
myfile.close()

# Reading the contents
with open("sample.txt", "r") as myfile:
    data = myfile.read()
    print(data)
```

Output:
```
Hello Python!
```

## Deep Dive:

Writing text files has been a fundamental practice in computer programming for decades. Before the rise of databases and other advanced data storage methods, text files were the go-to option for storing and organizing data. They are also useful for creating log files to track program activities and errors. Other methods like databases or document formats, such as XML or JSON, have their advantages in terms of structure and organization, but plain text files remain simple, lightweight, and easy to manipulate.

To create a new text file, the `open()` function is used, which takes in two arguments: the name of the file and the mode in which it will be opened. The `write()` method is used to add content to the file, and the `close()` method closes the file once the writing is complete. To read from a file, the `open()` function is again used, this time in read mode ("r"), and the `read()` method is used to retrieve the contents.

## See Also:

- [Python Text File Operations](https://www.w3schools.com/python/python_file_handling.asp)
- [Working with Files in Python](https://realpython.com/read-write-files-python/)
- [Understanding the open() function](https://www.programiz.com/python-programming/built-in-function/open)