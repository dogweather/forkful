---
title:    "Bash recipe: Reading a text file"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file is an essential skill for any Bash programmer. It allows you to access and manipulate the data stored in a file, giving you more control and flexibility in your scripts. Whether you're working with log files, configuration files, or user input, knowing how to read a text file is a valuable tool in your toolbox.

## How To
Reading a text file in Bash is a straightforward process. The first step is to open the file using the `cat` command. This will display the contents of the file in the terminal. For example, let's say we have a file named `data.txt` that contains the following text:

```
Hello
World
```

To read this file, we would use the following command:

```
cat data.txt
```

This will output the contents of the file as follows:

```
Hello
World
```

Alternatively, we could use the `less` command to read the file. Unlike `cat`, `less` allows us to scroll through the file if it's longer than one page. We can use the arrow keys or the spacebar to navigate through the file. To exit `less`, press the `q` key.

```
less data.txt
```

We can also use the `head` command to read the first few lines of a file, or the `tail` command to read the last few lines.

```
head data.txt
tail data.txt
```

## Deep Dive
Reading a text file in Bash involves more than just displaying its contents in the terminal. We can also capture the output of a command into a variable using the `$(command)` syntax. We can then use the variable in our script to perform further operations on the file's contents.

For example, let's say we want to count the number of lines in our `data.txt` file. We can use the `wc -l` command to achieve this. This command will output the number of lines in a given file.

```
lines=$(wc -l < data.txt)
echo "The number of lines in data.txt is $lines"
```

We can also use the `grep` command to extract specific information from a file. For instance, if our `data.txt` file contains both "Hello" and "World" on separate lines, we can use `grep` to filter only the lines containing "Hello."

```
grep "Hello" data.txt
```

This will output `Hello` as the only line in the terminal.

## See Also
- [Introduction to Bash scripting](https://www.shellscript.sh/)
- [Bash guide for beginners](https://linuxize.com/post/bash-scripting-tutorial-for-beginners/)
- [Bash scripting cheatsheet](https://devhints.io/bash)