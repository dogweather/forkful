---
title:                "Bash recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to create a file for a specific task in your Bash code, but didn't want to clutter up your directory with it? That's where creating temporary files comes in handy! By creating temporary files, you can store data temporarily and have it automatically deleted once you're done using it. This allows for a more organized and efficient coding experience.

## How To

Creating a temporary file in Bash is a simple process. First, we need to use the `mktemp` command. This command creates a temporary file or directory and prints its name. Let's see an example of how to use it:

```Bash
# Create a temporary file
temp_file=$(mktemp)

# Add some contents to the file
echo "Hello world" > $temp_file

# Print the contents of the file
cat $temp_file

# Delete the temporary file
rm $temp_file
```

Output:

```Bash
Hello world
```

In the above example, we first used the `mktemp` command to create a temporary file and assign its path to the `temp_file` variable. Then, we used the `echo` command to write "Hello world" to the file. Finally, we used the `cat` command to print the contents of the file and the `rm` command to delete the temporary file.

But what if we want to create a temporary file with a specific name or location? We can do that by adding a template to the `mktemp` command. For example:

```Bash
# Create a temporary file with a specific name
temp_file=$(mktemp my_temp_file.XXXXXX)

# Create a temporary file in a specific directory
temp_file=$(mktemp -p my_directory/)
```

For more options and variations of the `mktemp` command, you can use the `man` command in your terminal: `man mktemp`.

## Deep Dive

Behind the scenes, the `mktemp` command creates a unique file or directory name based on a template using the `XXXXXXXX` sequence. Each `X` represents a random character, ensuring that the file or directory name will always be unique. This is important because it prevents accidentally overwriting existing files or directories.

Additionally, the `mktemp` command creates the temporary file or directory with secure permissions, making it inaccessible to other users on the system. This is essential for security purposes and helps protect sensitive data that may be temporarily stored in the file.

## See Also

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [mktemp manual page](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)