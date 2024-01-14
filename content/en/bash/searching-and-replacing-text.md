---
title:    "Bash recipe: Searching and replacing text"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in a situation where you need to make a small but repetitive change in a large text file? Maybe you need to change a company name, or a product description, or even just fix a typo. In these situations, manually making the changes can be tedious and error-prone. This is where searching and replacing text using Bash programming can come in handy.

## How To

To search and replace text in Bash, we will be using the `sed` command. This command allows us to search for a specific pattern within a text file and replace it with another pattern. Let's see an example:

```
Bash

$ sed 's/old_pattern/new_pattern/g' input_file.txt > output_file.txt
```

In this command, the `s` tells `sed` that we want to perform a substitution, while the `/g` flag stands for global, meaning that all occurrences of the old pattern will be replaced. We also specify the input and output files, separated by the `>` symbol.

Let's say we have the following text in our input file:

```
This is a blog post about Bash programming. Bash is a popular scripting language used for automation and system administration.
```

And we want to replace all instances of "Bash" with "Python". Our sed command would look like this:

```
Bash

$ sed 's/Bash/Python/g' input_file.txt > output_file.txt
```

The output file would then contain the following text:

```
This is a blog post about Python programming. Python is a popular scripting language used for automation and system administration.
```

## Deep Dive

The `sed` command offers more control over the replacement process through additional flags. For example, we can use the `i` flag to ignore case, meaning that "Bash" and "bash" would both be replaced with "Python". We can also use the `w` flag to write the changes to the input file instead of creating a new output file. Furthermore, we can use regular expressions to specify more complex patterns for the search and replace process.

Another powerful tool for searching and replacing text in Bash is the `grep` command. This command allows us to search for a specific pattern and print only the matching lines. We can also combine `grep` with `sed` to make changes only to those matching lines.

## See Also

Here are some helpful resources for further learning and practice:

- [Bash Documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Sed Command Examples](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [Grep Commands Cheat Sheet](https://www.educative.io/blog/grep-commands-cheat-sheet)