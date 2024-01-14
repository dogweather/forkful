---
title:    "Python recipe: Writing a text file"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a simple task, but it can serve multiple purposes and be useful in various programming projects. In this blog post, we will explore the reasons why you might consider writing a text file and how to do it efficiently with Python.

## How To

Creating a text file in Python is a straightforward process. We will use the built-in `open()` function to create and write to a text file. Let's first see the syntax of this function:

```Python
open(filename, mode)
```

The `open()` function takes in two parameters: `filename` and `mode`. `filename` is the name of the text file you want to create or open, and `mode` determines the purpose of opening the file. There are four possible modes to open a file in Python:

- `'r'`: Read mode - This is the default mode used to open a file for reading.
- `'w'`: Write mode - This mode opens a file for writing. If the file already exists, it will be overwritten; if it doesn't exist, a new file will be created.
- `'a'`: Append mode - Similar to write mode, but the new content will be added at the end of the file.
- `'r+'`: Read and write mode - This mode allows you to read and write to a file at the same time.

Let's see an example of using the `open()` function to create a new text file and write some content to it:

```Python
with open('new_file.txt', 'w') as f:
    f.write("Hello, world!")
```

In this example, we have created a new text file named `new_file.txt` and opened it in write mode (`'w'`). The `with` statement ensures that the file is properly closed after being used. Inside the `with` block, we use the `write()` method to add the text "Hello, world!" to the file. Let's see the output of this code:

```
Hello, world!
```

As you can see, the `write()` method has successfully added the text to the file. Now, let's explore some other ways of writing to a text file.

If you already have some content in a variable and want to write it to a text file, you can use the `write()` method with string formatting, like this:

```Python
name = "John"
age = 30
with open('new_file.txt', 'w') as f:
    f.write("My name is %s and I am %d years old." % (name, age))
```
The output in the text file will be:
```
My name is John and I am 30 years old.
```

Another useful method for writing to a file is `writelines()`. It can take in a list of strings and write them to a file, each on a separate line. Let's see an example:

```Python
lines = ["This is line 1.\n", "This is line 2.\n", "This is line 3.\n"]
with open('new_file.txt', 'w') as f:
    f.writelines(lines)
```

The output in the text file will be:
```
This is line 1.
This is line 2.
This is line 3.
```

## Deep Dive

Now that we have seen the basics of creating and writing to a text file, let's take a deep dive into some other useful things you can do with text files in Python.

### Appending to a file

We have seen earlier that the `'a'` mode can be used to append new content to the end of a file. Let's see an example:

```Python
with open('existing_file.txt', 'a') as f:
    f.write("This line will be added at the end.")
```

The previous content of the file will remain unchanged, and the new line will be added at the end.

### Reading from a file

To read from a text file in Python, we can use the `read()` or `readlines()` methods. The `read()` method will return the entire contents of the file as a string, while `readlines()` will return a list of strings, each representing a line from the file. Let's see an example:

```Python
# Using read()
with open('existing_file.txt', 'r') as f:
    content = f.read()
    print(content)

# Using readlines()
with open('existing_file.txt', 'r') as f:
    lines = f.readlines()
    print(lines)
```

The output will be:
```
This is line 1.
This is line 2.
This is line 3.
```

**Note:** Remember to close the file after reading using the `close()` method to prevent any issues