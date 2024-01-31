---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Python means saving strings to a file on your disk. Programmers do this to persist data between sessions, log information, or export readable results.

## How to:

Writing to a file is simple. Use the `with` statement to open a file, then call `write()`.

```Python
# Writing to a file in Python
with open('example.txt', 'w') as file:
    file.write("Hello, World!")

# Reading the file back
with open('example.txt', 'r') as file:
    print(file.read())
```

Sample Output:
```
Hello, World!
```

Appending to an existing file without overwriting:

```Python
# Appending to a file in Python
with open('example.txt', 'a') as file:
    file.write("\nSee you later, World!")

# Reading the appended file
with open('example.txt', 'r') as file:
    print(file.read())
```

Sample Output:
```
Hello, World!
See you later, World!
```

## Deep Dive

Text file writing has roots in early computer systems. It's the most basic form of data persistence and interchange among programs and systems. While alternatives like databases exist for complex data, text files are widely used for their simplicity and human readability. When writing files, Python handles many complexities, such as buffering and memory management, behind the scenes and provides various modes (e.g., write 'w', append 'a') for different use cases.

## See Also

- Python's official documentation on file I/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Real Python guide on file I/O: https://realpython.com/read-write-files-python/
- Further reading on file handling in Python with context managers: https://docs.python.org/3/reference/compound_stmts.html#the-with-statement
