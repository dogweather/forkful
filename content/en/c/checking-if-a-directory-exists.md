---
title:                "C recipe: Checking if a directory exists"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered if a directory exists before creating a new one? Checking if a directory exists is an important aspect of programming, especially in situations where you want to avoid overwriting existing files or simply need to confirm the presence of a specific directory.

## How To

In C programming, the `opendir()` function can be used to check if a directory exists. First, a directory path is passed as a parameter to the function. If the directory exists, the function will return a pointer to the directory. If not, it will return NULL.

```
C #include <stdio.h>
#include <dirent.h>

int main()
{
    DIR *dir = opendir("/path/to/directory");

    if (dir != NULL) {
        printf("Directory exists!");
        closedir(dir);
    } else {
        printf("Directory does not exist!");
    }

    return 0;
}
```

Sample output when the directory exists:

```
Directory exists!
```

Sample output when the directory does not exist:

```
Directory does not exist!
```

## Deep Dive

The `opendir()` function uses the `stat()` system call behind the scenes to check if a directory exists. This system call checks the file or directory information and returns it to the calling program. If the file or directory does not exist, an error message is returned.

It is also worth noting that the `opendir()` function only checks if a directory exists, and not if it is a valid directory. Therefore, it is important to also handle any possible errors that may occur while trying to access the directory.

## See Also

- `opendir()` function documentation: https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html#Opening-a-Directory
- `stat()` system call documentation: https://man7.org/linux/man-pages/man2/stat.2.html