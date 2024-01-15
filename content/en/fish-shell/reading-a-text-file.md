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

## Why

If you're a programmer or someone who works with command-line tools, you may have come across the need to read and manipulate text files. Whether it's parsing data or creating automated tasks, being able to read a text file using Fish Shell can greatly improve your efficiency and productivity.

## How To

To read a text file using Fish Shell, follow these steps:

1. Open your Fish Shell terminal.
2. Navigate to the directory where the text file is located using the `cd` command.
3. Use the `cat` command followed by the name of the text file to display its contents in the terminal. For example: 
```Fish Shell
cat sample.txt
```
4. Alternatively, you can use the `less` command to view the text file in a more user-friendly format. This allows you to scroll through the file and search for specific words or phrases. For example:
```Fish Shell
less sample.txt
```
5. You can also use the `head` or `tail` commands to show the first or last few lines of the text file, respectively. For example:
```Fish Shell
head sample.txt
tail sample.txt
```

## Deep Dive

There are various ways to read a text file in Fish Shell, and each of these commands has additional options that allow you to manipulate the output. For example, the `cat` command has the `-n` option which displays line numbers, and the `less` command has the ability to search for specific words using the `/` key.

Additionally, you can use the output of these commands in conjunction with other Fish Shell commands to perform further actions on the text file. For example, you can pipe the output of `cat` or `less` to `grep` to search for specific patterns, or use `awk` to extract specific columns from a text file.

Overall, being familiar with how to read and manipulate a text file using Fish Shell can greatly enhance your command-line skills and make you a more efficient programmer.

## See Also
- [Fish Shell Official Documentation](https://fishshell.com/docs/current/)
- [A Beginner's Guide to Fish Shell](https://dev.to/siddharth/latest-dev-tips-introduction-to-fish-shell-3445)
- [10 Useful Tips for Using the Fish Shell](https://www.tecmint.com/fish-tips-and-tricks/)