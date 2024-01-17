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

## What & Why?
Checking if a directory exists is a common task in programming, especially in the world of Bash. It is the process of verifying whether a specific directory exists or not, and is necessary for proper program functionality. Programmers do this to ensure that their code runs smoothly and to prevent any errors or unexpected behavior.

## How to:
To check if a directory exists in Bash, you can use the ```test``` command. This command allows you to perform a variety of tests on files and directories, including checking for their existence. To use it, simply type the following command in your terminal:

```
test -d <directory_path>
```

If the directory exists, this command will return a successful exit status code of 0. Otherwise, it will return a status code of 1, indicating that the directory does not exist. You can also use the ```[...]``` syntax, which is essentially the same as the ```test``` command. Here is an example:

```
[ -d <directory_path> ]
```

Additionally, you can use the ```-e``` flag to check for the existence of any file or directory, not just directories. For example:

```
test -e <file_or_directory_path>
```

## Deep Dive:
In the early days of Bash, the only way to check for the existence of a directory was to use the ```test``` command or the ```[...]``` syntax. However, with the development of newer versions of Bash, the ```[ ... ]``` double square brackets syntax was introduced, providing more functionality and flexibility in conditional expressions. This syntax also became the preferred method for checking if a directory exists.

An alternative approach to checking for the existence of a directory is to use the ```[[ ... ]]``` syntax, which is an augmented version of the ```[ ... ]``` syntax. This syntax is more versatile and supports advanced conditionals, including logical operators and regular expressions. In comparison, the ```test``` command only supports basic conditionals.

## See Also:
To learn more about checking for the existence of directories in Bash, check out the following resources:
- [Bash Beginner's Guide on Directory Manipulation](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Linuxize tutorial on Bash File Test Operators](https://linuxize.com/post/bash-check-if-file-exists/)
- [Bash Reference Manual for Conditional Constructs](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Conditional-Constructs)