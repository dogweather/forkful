---
title:                "Reading a text file"
html_title:           "Fish Shell recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file simply means accessing the contents of a file in a human-readable form. Programmers often do this to extract important information or manipulate the data in some way.

## How to:

To read a text file in Fish Shell, use the built-in `read` command followed by the name of the file. For example:

```Fish Shell
read myfile.txt
```

This will print out the contents of `myfile.txt` in the console. To save the output to a variable, use the `-z` flag:

```Fish Shell
set text (read -z myfile.txt)
```

Now you can use the variable `text` in your code for further manipulation.

## Deep Dive:

Reading text files has been a common task for programmers since the early days of computing. It allows for easy manipulation of data without having to manually input it into the code. Other alternatives for reading files include using system calls or external programs, but the `read` command in Fish Shell provides a simple and efficient solution.

When reading a text file, Fish Shell reads the contents of the file line by line and prints them out in the console. The `-z` flag tells Fish to store the contents of the file in a variable rather than printing it out. This is useful for storing data that needs to be processed or used later in the code.

## See Also:

- [Fish Shell documentation on `read` command](https://fishshell.com/docs/current/commands.html#read)
- [GeeksforGeeks article on reading a text file in Fish Shell](https://www.geeksforgeeks.org/how-to-read-a-text-file-in-fish-shell/)
- [Official Fish Shell website for more information and resources](https://fishshell.com/)