---
title:    "Bash recipe: Reading a text file"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

When working with Bash programming, it is common to encounter text files that need to be read and manipulated. Knowing how to properly read a text file is an essential skill for any programmer, as it allows for efficient processing and extraction of data.

## How To

To begin, let's create a simple text file called "example.txt" with the following contents:

```
Hello
World
```

To read this file in Bash, we will use the `cat` command. The `cat` command is used to display the contents of a file and can be used as follows:

```Bash
cat example.txt
```

Running this command will output the contents of the file, like this:

```
Hello
World
```

This is a simple way to read a text file, but what if we want to store the data from the file in a variable? We can use the `$( )` notation to assign the output of a command to a variable. For example:

```Bash
my_variable=$(cat example.txt)
```

Now the contents of the file will be stored in the `my_variable` variable. We can verify this by using the `echo` command to display the variable:

```Bash
echo $my_variable
```

The output will be:

```
Hello
World
```

We can also use the `read` command to read a text file line by line. This is useful when dealing with larger files. For example, we can use the `while` loop to loop through each line of the file and perform some action on each line. Here's an example:

```Bash
while read line; do
    echo $line
done < example.txt
```

This will output each line of the file separately, like this:

```
Hello
World
```

## Deep Dive

When working with text files in Bash, it's important to note the difference between reading the file with `cat` and `read`. When using `cat`, the entire file is read at once and stored in memory, which can be problematic for large files. However, when using `read`, only one line is read at a time, allowing for more efficient processing of large files.

There are also many useful options available for the `cat` and `read` commands, such as skipping lines or reading from a specific position in the file. It's worth exploring these options to see how they can improve your text file reading experience in Bash.

## See Also

- [Bash Scripting Tutorial - Reading Files](https://linuxconfig.org/bash-scripting-tutorial-part-2-reading-and-writing-data)
- [How to Read a Text File in Bash](https://www.baeldung.com/linux/read-text-file-bash) 
- [Bash Beginners Guide - Reading and Writing Files](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)