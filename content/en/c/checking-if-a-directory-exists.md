---
title:                "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Have you ever encountered a situation where you needed to perform a certain operation in your code, but only if a specific directory exists? Checking for the existence of a directory is an essential task in programming, as it allows your code to anticipate and handle potential errors or unexpected scenarios.

## How To
To check if a directory exists in C, we can use the `opendir()` function from the `dirent.h` header. This function takes in the name of the directory as its parameter and returns a `DIR` pointer if the directory exists, or `NULL` if it does not. Here's an example code snippet:

```C
#include <stdio.h>
#include <dirent.h>

int main()
{
    // Specify the directory name to check
    char *dir_name = "/home/user/Desktop";

    // Use opendir() to check for the directory's existence
    DIR *dir = opendir(dir_name);

    // Check if the directory exists
    if(dir)
    {
        printf("Directory %s exists!\n", dir_name);
        closedir(dir);
    }
    else
    {
        printf("Directory %s does not exist!\n", dir_name);
    }

    return 0;
}
```

Sample output:
```
Directory /home/user/Desktop exists!
```

## Deep Dive
Behind the scenes, the `opendir()` function actually uses the `access()` system call to check for the existence of the directory. This system call takes in two parameters: the path to the directory and an integer representing the permissions we want to check for (in this case, we use the `F_OK` constant to simply check if the path exists). If the directory exists, the system call returns a value of 0, and if it does not exist, it returns `-1` and sets the `errno` variable to `ENOENT` (indicating an error of "no such file or directory").

Also, it's worth noting that `opendir()` only checks for the existence of the directory, not whether or not the current user has permission to access it. This is where the `access()` system call can come in handy, as it also allows us to check for read, write, and execute permissions.

## See Also
- [opendir documentation](https://www.man7.org/linux/man-pages/man3/opendir.3.html)
- [access documentation](https://www.man7.org/linux/man-pages/man2/access.2.html)
- [List of errno values](https://www.man7.org/linux/man-pages/man3/errno.3.html)