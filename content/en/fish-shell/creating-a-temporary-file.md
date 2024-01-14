---
title:    "Fish Shell recipe: Creating a temporary file"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Why
Creating temporary files is a common task in programming, especially when dealing with large amounts of data or complex processes. Temporary files allow us to store and manipulate data temporarily before either discarding them or writing them to a more permanent location. In this blog post, we will explore how to create temporary files using the Fish Shell programming language.

# How To
Fish Shell provides a convenient way to create temporary files using the `mktemp` command. This command creates a temporary file and returns the path to that file. Let's see an example of how to use it:

```
Fish Shell code block:
```Fish Shell
# Create a temporary file and store its path in a variable
set temp_file (mktemp)

# Output the path to the file
echo $temp_file
```

Output:
```
/var/folders/ty/dklqf3kd6bxfvsghwxdqq0fw0000gn/T/tmp.9ItiOgB3

```

As we can see in the output, the `mktemp` command has created a temporary file in the `/var/folders/ty/dklqf3kd6bxfvsghwxdqq0fw0000gn/T/` directory and the path to that file is stored in the `temp_file` variable.

We can also specify a prefix for the temporary file name using the `-p` flag. For example:

```
Fish Shell code block:
```Fish Shell
# Create a temporary file with a custom prefix
set temp_file (mktemp -p prefix_)

# Output the path to the file
echo $temp_file
```

Output:
```
/var/folders/ty/dklqf3kd6bxfvsghwxdqq0fw0000gn/T/prefix_RivDC6J

```

This will create a temporary file with the prefix "prefix_" followed by a randomly generated string.

# Deep Dive
Behind the scenes, the `mktemp` command uses the `mkstemp()` function from the C standard library. This function creates a temporary file with a unique name and returns a file descriptor for that file. Fish Shell then uses this file descriptor to create a symlink with the specified prefix and return its path.

It is worth noting that temporary files are not automatically deleted after use in the Fish Shell. It is the responsibility of the programmer to manually delete them when they are no longer needed. This can be done using the `rm` command.

# See Also
- [Fish Shell official documentation on `mktemp`](https://fishshell.com/docs/current/cmds/mktemp.html)
- [C Standard Library documentation on `mkstemp`](https://www.cplusplus.com/reference/cstdio/mkstemp/)

Creating temporary files is a useful skill to have in your programming arsenal. Whether you are working with large datasets or performing complex operations, temporary files can help you manage and manipulate your data with ease. Give it a try in Fish Shell and see how it can improve your coding experience!