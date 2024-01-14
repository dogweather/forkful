---
title:    "Fish Shell recipe: Reading a text file"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Why
In any programming language, the ability to read and manipulate text files is an essential skill for a developer. In this blog post, we will explore how to read a text file using the Fish Shell. By understanding this functionality, you can efficiently handle large amounts of data, automate tasks, and create powerful scripts.

# How To
Reading a text file using Fish Shell is a straightforward process. The following code snippet will read a file named "example.txt":

```Fish Shell
set file (cat example.txt)
echo $file
```

The first line uses the `cat` command to read the contents of the file and store them in the variable `file`. The second line then uses the `echo` command to display the content of the file on the terminal. This simple process can be used for any text file, regardless of its size or format.

If you want to read a specific line from a file, you can use the `head` or `tail` command. For example, if you want to read the first five lines of the file, you can use the following code:

```Fish Shell
set lines (head -n 5 example.txt)
echo $lines
```

Similarly, if you want to read the last five lines of the file, you can use the `tail` command instead. Additionally, you can also use the `wc` command to count the number of lines, words, or characters in a text file.

# Deep Dive
The `cat` command used in the first example is not the only way to read a text file in Fish Shell. You can also use the `read` command, which allows you to read a file line by line. This is particularly useful if you want to perform different actions on each line of the file.

Another important aspect to consider when reading a text file is its encoding. By default, Fish Shell uses UTF-8 encoding, but you can specify a different encoding using the `locale` command. This is especially important if you are working with files in different languages or with special characters.

# See Also
- [Fish Shell documentation on reading files](https://fishshell.com/docs/current/index.html#input-output)
- [Examples of using the `cat` command in Fish Shell](https://www.geeksforgeeks.org/fish-command-linux-examples/)