---
title:    "Bash recipe: Checking if a directory exists"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

In Bash programming, it is important to check if a directory exists before performing any operations on it. This ensures that the code runs smoothly without any errors, as it will avoid trying to access a non-existent directory.

## How To

To check if a directory exists in Bash, we can use the `test` command with the `-d` option, followed by the name of the directory we want to check. For example:

```Bash
test -d my_directory
```

This will return an exit status of 0 if the directory exists, and 1 if it doesn't. We can also use the `&&` operator to perform an action if the directory exists, and the `||` operator to perform an action if it doesn't.

```Bash
test -d my_directory && echo "Directory exists!" || echo "Directory doesn't exist!"
```

If you want to also check if the directory is readable, writable, or executable, you can use the `-r`, `-w`, and `-x` options respectively. For example:

```Bash
test -r my_directory && echo "Directory is readable!"
```

## Deep Dive

While the `test` command is sufficient for basic checks, there is another command in Bash that is specifically designed for checking files and directories - the `file` command. This command can be used to check if a file is a directory, and it also provides additional information such as the file type and permissions.

```Bash
file my_directory
```

This will return something like `my_directory: directory`. We can also use the `grep` command to filter the output and check for specific information. For example, if we only want to check if the directory exists, we can use the `-s` option to suppress the additional information and then use `grep` to search for the word "directory".

```Bash
file -s my_directory | grep -q directory && echo "Directory exists!"
```

## See Also

- [Bash Learning Series](https://www.pluralsight.com/blog/it-ops/linux-shell-chain-operators-tips)
- [Bash Scripting Tutorial for Beginners](https://www.shellscript.sh/index.html)
- [The `file` command in Bash](https://www.geeksforgeeks.org/file-command-in-linux-with-examples/)