---
title:    "Fish Shell recipe: Reading a text file"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Have you ever wanted to quickly and easily read through a text file in your Fish Shell? Maybe you need to check for certain patterns or extract specific information from the file. Well, with a few simple commands, you can easily read and manipulate text files in Fish Shell.

## How To

To read a text file in Fish Shell, you can use the `cat` command. This command will print the contents of the file directly in your terminal window, allowing you to easily read through it.

```Fish Shell
cat my_file.txt
```

If you want to search for a specific pattern in the file, you can use the `grep` command along with `cat`. For example, let's say you want to find all lines in the file that contain the word "apple". You can use the following command:

```Fish Shell
cat my_file.txt | grep 'apple'
```

This will only print out the lines that contain the word "apple" in the file.

If you need to specify a specific line or section of the file to read, you can use the `head` or `tail` commands. `head` will print out the first few lines of the file, while `tail` will print out the last few lines. You can also specify the number of lines you want to print using the `-n` flag.

```Fish Shell
head -n 10 my_file.txt  # will print the first 10 lines of the file
tail -n 5 my_file.txt  # will print the last 5 lines of the file
```

Finally, if you need to extract specific information from a file, you can use `awk`. This command gives you more control over how the file is read and allows you to extract specific fields or columns of data.

```Fish Shell
awk '{print $2}' my_file.txt  # will print the second column of data in the file
awk '/apple/{print $1, $3}' my_file.txt  # will print the first and third columns of data for any line containing the word "apple"
```

## Deep Dive

For a deeper understanding of how text files are read in Fish Shell, it's important to understand the concept of "streams". When you use commands like `cat`, `grep`, and `awk`, you are essentially creating a stream of data that is being passed from one command to the next. This allows for more efficient and flexible manipulation of files.

Additionally, Fish Shell offers many features and options for reading text files, such as the ability to read and write to multiple files at once, perform string substitutions, and save the output to a new file. Learning these features can greatly enhance your text file reading abilities in Fish Shell.

## See Also

Here are some helpful resources for further reading on reading text files in Fish Shell:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix - Manipulating files](https://www.tutorialspoint.com/unix/unix-manipulating-files.htm)
- [Bash Guide for Beginners - File Manipulation](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_01.html)