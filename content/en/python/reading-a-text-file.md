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

## Why

Reading a text file is a common task in programming, especially when dealing with large amounts of data. By learning how to read a text file in Python, you can easily access and manipulate the data within it, making your code more efficient and effective.

## How To

To read a text file in Python, we will first need to open the file using the built-in `open()` function. This function takes two parameters: the path to the file and the mode in which we want to open the file. In this case, we will use the `"r"` mode, which stands for "read-only".

```Python
# Opening a file in "r" mode
file = open("example.txt", "r") 
```

Next, we can use the `read()` method to read the entire contents of the file and store it in a variable.

```Python
# Reading the contents of the file
data = file.read() 
```

We can also specify the number of characters we want to read using the `read()` method. This is useful when dealing with large files and we only need to process a specific portion of it.

```Python
# Reading 100 characters from the file
data = file.read(100) 
```

After we're done reading the file, we should always close it using the `close()` method to free up system resources.

```Python
# Closing the file
file.close() 
```

To make the code more efficient, we can use the `with` statement. This automatically closes the file once we're done with it.

```Python
# Using "with" statement to automatically close the file
with open("example.txt", "r") as file: 
    data = file.read()
```

Now, let's create a `example.txt` file with the following contents:

```
Hello World!
This is a text file.
```

When we run the above code, the `data` variable will contain the contents of the file as a string.

```
Hello World!
This is a text file.
```

## Deep Dive

When reading a text file, it's important to keep in mind the file's encoding. This is the way the characters are encoded and stored in the file. By default, Python uses the system's default encoding. However, we can specify the encoding using the `encoding` parameter in the `open()` function.

```Python
# Opening a file with a specific encoding
file = open("example.txt", "r", encoding="utf-8") 
```

Another useful method for reading text files is `readlines()`. This method returns a list where each element is a line from the file.

```Python
# Reading and storing each line as a separate element in a list
with open("example.txt", "r") as file: 
    data = file.readlines() 
```

We can also use a `for` loop to read the file line by line.

```Python
# Reading the file line by line using a "for" loop
with open("example.txt", "r") as file: 
    for line in file: 
        print(line) 
```

## See Also

- [Reading and Writing Files in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [File Input and Output in Python](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)