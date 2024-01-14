---
title:                "Bash recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer who works with Bash, there may come a time when you need to check if a directory exists. This could be useful, for example, in a script that needs to create a new directory only if the specified directory does not already exist.

## How To

To check if a directory exists in Bash, we can use the ```-d``` flag with the ```test``` command. The syntax would look like this:

```Bash
if [ -d <directory_path> ]; then
  echo "Directory exists!"
else
  echo "Directory does not exist!"
fi
```

Let's break down what's happening here. The ```test``` command is used to evaluate conditions and return a status code. The ```-d``` flag specifically checks if the given path is a directory. So, if the directory exists, the status code will be 0 (True) and the first statement will be executed. If the directory does not exist, the status code will be 1 (False) and the second statement will be executed.

Here's a sample output for a directory that exists:

```
Directory exists!
```

And here's a sample output for a directory that does not exist:

```
Directory does not exist!
```

You can also include this check in a conditional and use it to perform other actions, such as creating the directory if it doesn't exist. Here's an example:

```Bash
if [ ! -d <directory_path> ]; then
  mkdir <directory_path>
  echo "Directory created!"
fi
```

In this case, the ```!``` operator before the ```-d``` flag negates the condition, so the statement will only be executed if the directory does not exist.

## Deep Dive

Behind the scenes, the ```-d``` flag with the ```test``` command actually checks for the existence of a directory file descriptor. It does this by using a system call (```stat()``` in Linux) to retrieve information about the file. If the retrieved information indicates that the file is a directory, then the directory exists.

It's worth noting that the ```-d``` flag will also return 0 for symbolic links that point to directories. So if you need to check for the actual existence of a directory, you may need to add an additional check using the ```-L``` flag to exclude symbolic links. The syntax would look like this:

```Bash
if [ -d <directory_path> ] && [ ! -L <directory_path> ]; then
  # Do something
fi
```

## See Also

- [Bash test command documentation](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Linux stat system call documentation](https://man7.org/linux/man-pages/man2/stat.2.html)