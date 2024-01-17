---
title:                "Creating a temporary file"
html_title:           "Bash recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file in Bash is a way for programmers to quickly store and manipulate data without permanently saving it onto their system. This is useful for tasks such as storing error logs, processing large amounts of data, or creating temporary backups during a program's execution.

## How to:

To create a temporary file in Bash, you can use the `mktemp` command followed by the desired filename extension. For example, `mktemp example.txt` will create a temporary file named "example.txt" in the current working directory. You can then use this file in your Bash commands and scripts.

```Bash
mktemp example.txt
echo "This is a temporary file" > example.txt
cat example.txt
```
Output:
```
This is a temporary file
```

You can also use the `mktemp` command with the `-d` option to create a temporary directory instead of a file. This can be useful for organizing and storing multiple temporary files within the same directory.

## Deep Dive

Creating temporary files has been a common programming practice for a long time, and Bash is no exception. In the early days of computing, temporary files were often created and used as a way to process data and conserve system resources. With the introduction of more advanced programming languages and techniques, temporary files are still used for similar purposes.

An alternative to creating temporary files in Bash is to use pipes and redirection to store data in memory instead of writing it to a physical file. This can be more efficient for certain tasks, but temporary files provide a more permanent and tangible way to store and manipulate data.

When creating a temporary file, Bash uses the `mktemp` utility under the hood. This utility allows for more advanced options, such as specifying a specific directory for the temporary file to be created in, and generating unique filenames using a template. The `mktemp` command also has security features built-in to ensure that temporary files are not easily accessible by other users on the system.

## See Also

- [mktemp man page](https://linux.die.net/man/1/mktemp)
- [Understanding the tmp directory in Linux](https://opensource.com/article/19/4/understanding-tmp-directory-linux)