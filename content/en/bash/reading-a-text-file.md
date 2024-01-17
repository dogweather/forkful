---
title:                "Reading a text file"
html_title:           "Bash recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in Bash refers to accessing and extracting data from a file that contains characters and words, rather than binary code. Programmers often need to read text files in order to retrieve information or to modify and manipulate the contents of the file.

## How to:

To read a text file in Bash, you can use the `cat` command along with the file name as an argument. For example, `cat file.txt` will display the contents of the `file.txt` on the terminal.

To read a specific line of a text file, you can use the `sed` command with the `-n` option and specify the line number. For example, `sed -n 5p file.txt` will print the fifth line in the file.

You can also use the `head` and `tail` commands to read the first or last few lines of a text file. For example, `head -n 10 file.txt` will show the first 10 lines of the file.

## Deep Dive:

Historically, text files were created as a way to store data in a human-readable format. This made it easier for programmers to access and manipulate the information within the file without having to interpret binary code.

Alternatives to reading text files in Bash include using other programming languages such as Python or Java, which have built-in libraries for reading and parsing text files.

When reading a text file in Bash, it is important to understand how the file is structured in terms of characters, lines, and encoding. This can affect how the text is displayed and how it can be manipulated.

## See Also:

- [Bash Guide for Beginners](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash scripting cheat sheet](https://devhints.io/bash)