---
title:    "Fish Shell recipe: Creating a temporary file"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why 
Temporary files are an essential part of any programming language, they provide a way to store information temporarily without cluttering up your system's file structure. Creating a temporary file can be useful in a variety of situations, such as saving user input or storing data that is only needed for a short period of time. In this blog post, we will explore how to create temporary files in the Fish Shell programming language.

## How To
Creating a temporary file in Fish Shell is a straightforward process. Let's take a look at some coding examples and the corresponding sample output.

First, we need to use the `mktemp` command to create a temporary file. This command generates a unique filename and creates an empty file with that name. We can specify the file extension using the `-t` flag. For example, if we want to create a temporary file with a `.txt` extension, we can use the following command:

```Fish Shell
$ mktemp -t tmpfile.txt
```

This will create a temporary file with the name `tmpfile.txt` in the current directory. 

We can also specify the directory where we want the temporary file to be created using the `-d` flag. For example, if we want to create a temporary file in the `tmp` directory, we can use the following command:

```Fish Shell
$ mktemp -d tmp/dir
```

We can also add random characters to the file name using the `-u` flag. For example, if we want to create a temporary file with random characters in the name, we can use the following command:

```Fish Shell
$ mktemp -u tmpfile_XXXXXXX.txt
```

This will create a temporary file with a random string of characters in place of the `X`'s in the file name. 

Once we have created our temporary file, we can write or append data to it using the `echo` command. For example, to write the string "Hello World" to our temporary file, we can use the following command:

```Fish Shell
$ echo "Hello World" > tmpfile.txt
```

We can then view the contents of our temporary file using the `cat` command:

```Fish Shell
$ cat tmpfile.txt
Hello World
```

Once we are done using our temporary file, we can delete it using the `rm` command:

```Fish Shell
$ rm tmpfile.txt
```

## Deep Dive
Behind the scenes, the `mktemp` command uses the `mkstemp` function from the C standard library to create a temporary file. This function creates the file with read and write permissions for the current user. It also ensures that the file is unique by appending a random string of characters to the file name.

Additionally, the `mktemp` command creates the temporary file in a secure manner, making it difficult for other users to hijack the file or create a file with the same name. It does this by creating the file in a temporary directory with restricted permissions.

## See Also
- [Fish Shell official documentation on mktemp](https://fishshell.com/docs/current/commands.html#mktemp)
- [Linux man page for mktemp](https://linux.die.net/man/1/mktemp)
- [C documentation for mkstemp function](https://www.gnu.org/savannah-checkouts/gnu/libc/manual/html_node/Temporary-Files.html)