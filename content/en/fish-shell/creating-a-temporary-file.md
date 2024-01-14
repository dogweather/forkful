---
title:                "Fish Shell recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files in programming can be a useful tool for many tasks. It allows for storing and manipulating data without permanently affecting the original files. This can be helpful for tasks such as data processing, file management, or debugging.

## How To

To create a temporary file using Fish Shell, we can use the `mktemp` command. This command generates a unique temporary file name and creates an empty file with that name. Let's take a look at an example:

```
Fish Shell  >> mktemp -p ~/Desktop
~/Desktop/tmp.2piW7Exq
```

In this example, we used the `-p` flag to specify the directory where we want the temporary file to be created. If no directory is specified, the temporary file will be created in the system's default temporary directory.

We can also use the `mktemp` command to create multiple temporary files at once by specifying the number of files we want to create. For example, if we want to create three files, we can use the following command:

```
Fish Shell >> mktemp -p ~/Documents -t mytempfile_ -n 3
~/Documents/mytempfile_1
~/Documents/mytempfile_2
~/Documents/mytempfile_3
```

In this example, we used the `-t` flag to specify the prefix of the temporary file name and the `-n` flag to specify the number of files to create.

## Deep Dive

Behind the scenes, the `mktemp` command uses the `mkstemp` function from the C standard library to create the temporary files. This function creates a file with a randomly generated name that is hardcoded to be 6 characters long. If the name is already taken, it will keep generating different names until it finds an available one.

Once the file is created, the `mktemp` command outputs the full path to the file. This allows us to easily use the temporary file in our scripts or programs.

## See Also

- Official Fish Shell documentation for `mktemp` command: https://fishshell.com/docs/current/cmds/mktemp.html
- Learn more about the `mkstemp` function in C: https://man7.org/linux/man-pages/man3/mkstemp.3.html