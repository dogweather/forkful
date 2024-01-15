---
title:                "Checking if a directory exists"
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to check if a directory exists in your C program? Maybe you want to make sure a certain path exists before creating a file or performing other operations. Whatever the reason may be, knowing how to check for the existence of a directory is a useful skill to have in your programming arsenal.

## How To

To check if a directory exists in C, we can use the `opendir()` function from the `<dirent.h>` header file. This function takes in a path as its argument and returns a pointer to a `DIR` object if the directory exists, or `NULL` if it doesn't. Here's an example of how we can use this function in our code:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Change this path to the one you want to check
    char* path = "path/to/directory";
    DIR* dir = opendir(path);

    if (dir) {
        // Directory exists!
        printf("Directory at %s exists.\n", path);
        closedir(dir);
    } else {
        // Directory doesn't exist
        printf("Directory at %s doesn't exist.\n", path);
    }

    return 0;
}
```

If the directory at the given path exists, we will see the following output:

```
Directory at path/to/directory exists.
```

Otherwise, we will see:

```
Directory at path/to/directory doesn't exist.
```

## Deep Dive

Behind the scenes, the `opendir()` function uses the `stat()` system call to check for the existence of the specified path. This function returns information about a file, such as its size, permissions, and type. By using the `stat()` call, `opendir()` is able to determine if the path points to a valid directory or not.

It's also worth noting that the `DIR` object returned by `opendir()` is a pointer to a data structure containing information about the directory, such as its file descriptor, current position, and the list of files in the directory. This object is used by other functions like `readdir()` to traverse the contents of the directory.

## See Also

- [opendir(3) - Linux manual page](https://linux.die.net/man/3/opendir)
- [stat(2) - Linux manual page](https://linux.die.net/man/2/stat)
- [C Programming - File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)