---
title:                "Checking if a directory exists"
html_title:           "Bash recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

In Bash, it is common to check if a directory exists before performing any operations on it. This ensures that the script does not encounter any errors or unexpected behavior.

## How To

To check if a directory exists in Bash, we will use the `test` command with the `-d` option. This option checks if the given path is a directory or not. The syntax for this command is as follows:

```
if [ -d <directory_path> ]
then
    # execute your code here
else
    # directory does not exist
fi
```

You can also use the `[[` command instead of `test` for a more concise syntax:

```
if [[ -d <directory_path> ]]
then
    # execute your code here
else
    # directory does not exist
fi
```

To make this process even simpler, we can use the `&&` operator to combine the `test` command with the actual code we want to execute:

```
[[ -d <directory_path> ]] && <command_to_execute>
```

### Example:

```
if [ -d /home/user/documents ] 
then
    echo "Documents directory exists!"
else
    echo "Documents directory does not exist."
fi
```

In this example, we are checking if the "documents" directory exists in the home folder. If it does, the script will print "Documents directory exists!" Otherwise, it will print "Documents directory does not exist."

## Deep Dive

The `test` command with the `-d` option checks if the given path points to a directory, and only a directory. It will return false for any other type of file or path, including symbolic links that point to directories. 

If you want to check if a directory exists and follow symbolic links, you can use the `-L` option. This will return true if the given path points to a directory, or if it is a symbolic link that points to a directory.

Another useful option is `-e`, which checks if the given path exists, regardless of its type. This means it will return true for directories, files, and symbolic links.

### Example:

```
if [ -L /home/user/documents ] 
then
    echo "Symbolic link to documents found!"
else
    echo "Symbolic link to documents not found."
fi
```

In this example, we are checking if there is a symbolic link to the "documents" directory in the home folder. If it exists, the script will print "Symbolic link to documents found!" Otherwise, it will print "Symbolic link to documents not found."

## See Also

- [Bash scripting tutorial](https://www.shellscript.sh)
- [Bash reference manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Test command documentation](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)