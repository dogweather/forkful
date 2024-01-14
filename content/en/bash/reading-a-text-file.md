---
title:                "Bash recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file may seem like a simple task, but it is a crucial skill to have in any kind of programming. It allows you to access and manipulate data stored in a text format, which is commonly used for various purposes such as configuration files, data storage, and more.

## How To

In Bash programming, reading a text file can be done using the `cat` command. This command takes in the name of the text file as an argument and outputs the contents of the file to the terminal. Let's say we have a text file called "example.txt" with the following content:

```Bash
Hello World!
This is an example text file.
```

To read this file, we can use the `cat` command as follows:

```Bash
cat example.txt
```

This will output the contents of the file to the terminal, which in this case will be:

```Bash
Hello World!
This is an example text file.
```

We can also use the `read` command to read a text file line by line. This command can be used in a loop to read and process data from a text file. Let's see an example of this in action:

```Bash
while read line; do
	echo "The line says: $line"
done < example.txt
```

Running this code will output each line of the text file with the added prefix "The line says:".

## Deep Dive

Reading a text file can be done in multiple ways. As shown above, the `cat` and `read` commands are the most popular ways to read a text file in Bash. However, there are other commands and options available as well.

For example, the `head` and `tail` commands allow you to read the first or last few lines of a text file, respectively. The `less` command allows you to view the contents of a text file with the ability to scroll and search through the file. And the `grep` command can be used to search for specific words or patterns in a text file.

Additionally, Bash also allows you to read a text file directly into a variable, using the `$(<filename)` syntax. This can be useful when you need to use the contents of a text file in your code.

## See Also

- [Bash Reference Manual - Basic Concepts](https://www.gnu.org/software/bash/manual/html_node/Basic-Shell-Concepts.html)
- [Linux Command Library - Bash Commands](https://www.linuxcommand.org/lc3_learning_the_shell.php)
- [Bash Scripting Tutorial - Working with Files](http://www.panix.com/~elflord/unix/bash-tute.html#3.1)