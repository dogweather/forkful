---
title:    "Python recipe: Writing a text file"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Writing a text file is a fundamental aspect of programming as it allows us to store data in a structured and readable format. Text files are commonly used for various purposes such as storing user input, logging data, and saving configurations. In this blog post, we will explore the basics of writing a text file in Python.

## How To

To create a text file in Python, we first need to open a file object by using the `open()` function. This function takes in two arguments: the file name and the mode in which the file will be opened. Mode `w` is used for writing to the file. We can also specify the encoding of the file if needed.

```
file = open("example.txt", "w", encoding="utf-8")
```

Next, we can use the `write()` method to actually write to the file. This method takes in a string as an argument and writes it to the file. We can also use formatting to insert variables into the string.

```
username = "John"
file.write("Hello, my name is %s." % username)
```

After we have finished writing to the file, we need to close the file object using the `close()` method. This is an important step as it saves any changes made to the file.

```
file.close()
```

If we want to append more content to the file without overwriting the existing content, we can use mode `a` instead of mode `w`.

```
file = open("example.txt", "a")
file.write(" I am learning Python.")
file.close()
```

To ensure that the file is closed even if an error occurs, we can use the `with` statement. This automatically closes the file when the code within the statement has finished executing.

```
with open("example.txt", "w") as file:
    file.write("This is another example.")
```

## Deep Dive

There are a few important concepts to keep in mind when writing a text file in Python. One is the use of the newline escape character `\n` which adds a new line to the file. We can also use the `write()` method multiple times to add multiple lines to the file.

Another important aspect to consider is the encoding of the text file. While most text files are encoded in ASCII, it is recommended to use UTF-8 encoding to support different languages and characters.

Lastly, we can also use the `read()` method to read the contents of a text file. This method returns a string containing all the contents of the file.

```
file = open("example.txt", "r")
contents = file.read()
file.close()
print(contents)
```

## See Also

- [Python file modes](https://www.programiz.com/python-programming/file-operation#modes)
- [Python string formatting](https://www.programiz.com/python-programming/string-interpolation)
- [Python encoding](https://www.programiz.com/python-programming/encoding)
- [Python file handling tutorial](https://www.programiz.com/python-programming/file-operation)