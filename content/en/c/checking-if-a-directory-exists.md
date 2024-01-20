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

## What & Why?
If you're writing a C program that interacts with the file system, you need to know if a directory exists before performing operations on it. This is preventive error handling: you stop bugs before they mess up your user's data.

## How to:
In C, we use the `stat` function and `S_ISDIR` macro to check if a directory exists. This works for both Unix-based systems (like Linux and MacOS) and Windows.

```C
#include <sys/stat.h>
#include <stdio.h>

int main() {
    struct stat status = {0};
    const char *dirPath = "./testdirectory";
    if(stat(dirPath, &status) == -1 || !S_ISDIR(status.st_mode)) {
        printf("Directory does not exist\n");
    }
    else {
        printf("Directory exists\n");
    }

    return 0;
}
```

## Deep Dive
The `stat` function has been a part of Unix-based systems since the 70s, allowing programmers to access file or directory properties. Passed the correct path, it populates a `struct stat` with information about the file or directory. Windows implements `stat` as part of the POSIX compatibility in its C library.

`S_ISDIR` is a POSIX macro that uses the `st_mode` field of `struct stat` to determine if the path belongs to a directory.

But we have alternatives too. `opendir` function, for an instance, tries to open a directory and will return `NULL` if it doesn't exist or can't be read.

```C
#include <dirent.h>
#include <stdio.h>

int main() {
    const char *dirPath = "./testdirectory";
    DIR* dir = opendir(dirPath);
    if (dir) {
        printf("Directory exists\n");
        closedir(dir);
    }
    else {
        printf("Directory does not exist\n");
    }

    return 0;
}
```
This works perfectly well, but it's best to only use it if you actually want to read the directory. For merely checking if it's there, `stat` uses fewer resources.

## See Also
For more info, see the official documentation on [`stat`](https://man7.org/linux/man-pages/man2/stat.2.html), [`S_ISDIR`](https://linux.die.net/man/3/s_isdir), and [`opendir`](https://linux.die.net/man/3/opendir). To deepen your understanding, engage in [this](https://stackoverflow.com/questions/4553012/checking-if-a-file-is-a-directory-or-just-a-file) StackOverflow discussion.