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

## Why
Creating temporary files in Bash can be useful for a variety of purposes, such as temporary data storage, logging, or manipulating large amounts of data. Temporary files are typically automatically deleted once the process using them is finished, reducing clutter on your system.

## How To
To create a temporary file in Bash, you can use the `mktemp` command. This will create a unique and secure temporary file in the system's temporary directory. The syntax is as follows:
```
Bash
#!/bin/bash
temp_file=$(mktemp)
```
To use the temporary file, you can then append data to it in your script:
```
Bash
echo "This is a temporary file" >> $temp_file
```
You can also specify a prefix for the temporary file name, which can be helpful for organization and clarity:
```
Bash
#!/bin/bash
temp_file=$(mktemp -t my_temp_file)
```
Once you have finished using the temporary file, you can remove it with the `rm` command:
```
Bash
rm $temp_file
```
To view the contents of the temporary file, you can use the `cat` command:
```
Bash
cat $temp_file
```

## Deep Dive
Behind the scenes, the `mktemp` command creates a secure temporary file using the system's default temporary directory (usually /tmp). The file is given a unique name based on the current time, process ID, and a random number to ensure it is truly unique. This makes it less likely for multiple processes to accidentally use the same temporary file. Additionally, the temporary file has restrictive permissions, making it readable and writable only by the user who created it.

It is important to note that the `mktemp` command does not create the actual file, but rather creates a placeholder for the file. The actual file is not created until data is written to it using a command like `echo` or `printf`.

See Also
- [Bash Man Page - `mktemp` command](https://www.gnu.org/software/bash/manual/html_node/Creating-Temporary-Files.html) 
- [Linuxize - How to create a temporary file in Bash](https://linuxize.com/post/how-to-create-temporary-files-in-bash/)
- [The Linux Command Line - Shell Script Basics](http://linuxcommand.org/lc3_writing_shell_scripts.php)