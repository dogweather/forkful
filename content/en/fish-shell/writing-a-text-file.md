---
title:    "Fish Shell recipe: Writing a text file"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file may seem like a simple task, but it can be a powerful tool for automating tasks in your workflow. With the Fish Shell, you can easily create and edit text files using simple commands, making your programming experience more efficient and organized. 

## How To
Creating a text file in Fish Shell is as easy as using the `echo` command. For example, to create a file named "hello.txt" with the content "Hello World!", you would use the following command:

```Fish Shell
echo "Hello World!" > hello.txt
```

To add more text to the file, you can use the append `>>` operator. For example, to add the line "Welcome to my text file!" to the existing "hello.txt" file, you would use:

```Fish Shell
echo "Welcome to my text file!" >> hello.txt
```
You can also use the `cat` command to display the contents of a text file in your terminal. For example, to display the content of the "hello.txt" file, you would use:

```Fish Shell
cat hello.txt
```

## Deep Dive
When writing a text file, it's important to use the correct file encoding to ensure that your text is displayed correctly. Fish Shell uses UTF-8 encoding by default, which supports a wide range of characters and symbols. You can also change the file encoding by adding the `--eol-style` flag to your `echo` command. For example, to create a file with UTF-16 encoding, you would use:

```Fish Shell
echo --eol-style utf-16 "Hello World!" > hello.txt
```

You can also use variables in your text file by using the `$variable_name` syntax. For example, if you have a variable named `name` with the value "John", you can include it in your text file by using:

```Fish Shell
echo "Hi $name, welcome to my text file!" > hello.txt
```

## See Also
- Official Fish Shell documentation on [redirecting output to files](https://fishshell.com/docs/current/tutorial.html#tut_std) 
- Tutorial on [working with text files in Fish Shell](https://www.linode.com/docs/guides/how-to-work-with-files-in-fish-shell/) 
- Documentation for [Fish Shell's `cat` command](https://fishshell.com/docs/current/cmds/cat.html)