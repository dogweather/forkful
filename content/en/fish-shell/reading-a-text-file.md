---
title:                "Fish Shell recipe: Reading a text file"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, working with text files is a common and essential task, whether for data processing, log analysis, or configuration management. In this blog post, we will explore how to efficiently read a text file using Fish Shell, a modern and user-friendly shell for Linux and Unix systems. 

## How To

To read a text file in Fish Shell, we will use a combination of built-in shell commands and the powerful text processing utility, `sed`. Let's look at an example:

```Fish Shell
set text_file "sample.txt"
cat $text_file | sed -n "3,6p"
```

In the first line, we set a variable named `text_file` with the name of our text file. Then, using the `cat` command, we pipe the contents of the text file to `sed`, which applies the specified `sed` command to print only lines 3 to 6 of the file. 

The output of the above code would be the following:

```
This is line 3.
This is line 4.
This is line 5.
This is line 6.
```

But what if we want to print the contents of the entire file? For that, we can use a wildcard character `*` to select all lines:

```Fish Shell
cat $text_file | sed -n "*p"
```

This command will print the entire contents of the text file. 

## Deep Dive

Now, let's break down the command we just used to read the text file. 

First, `cat` is a Fish Shell command that prints the contents of a file to the standard output. It can also be used to concatenate multiple files together. 

Next, we use the `sed` command with the `-n` flag to suppress automatic printing of pattern space. This is useful when we only want to print specific lines of a file. 

The `-n` flag is followed by the `sed` command `"3,6p"`, where `3` represents the starting line number and `6` represents the ending line number. The `p` command within the quotes indicates that we want to print the specified range of lines. 

## See Also

For more information on Fish Shell commands and `sed`, check out the official documentation for [Fish Shell](https://fishshell.com/docs/current/index.html) and [sed](https://www.gnu.org/software/sed/manual/sed.html). 

Happy coding!