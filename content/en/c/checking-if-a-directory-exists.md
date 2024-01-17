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
Checking if a directory exists is the process of verifying if a given directory (folder) exists on a computer's file system. This is commonly done by programmers to ensure that a certain directory is present before performing relevant operations or to handle error cases when a directory is missing.

## How to:
To check if a directory exists in C, we can use the ```opendir()``` function from the ```<dirent.h>``` header. This function opens a directory stream and returns a pointer to the ```DIR``` type, which represents a directory stream. If the directory does not exist, the function returns a ```NULL``` pointer.

```
#include <stdio.h>
#include <dirent.h>

int main() {
    char* directory_name = "my_directory";
    DIR* dir = opendir(directory_name);
    
    if (dir == NULL) {
        printf("Directory \"%s\" does not exist.\n", directory_name);
    } else {
        printf("Directory \"%s\" exists.\n", directory_name);
    }
    
    closedir(dir);
    return 0;
}
```

Sample Output:
```
Directory "my_directory" does not exist.
```

## Deep Dive:
In C, the ```opendir()``` function is part of the POSIX standard and has been available since the first version of the C programming language. It is commonly used on Unix and Unix-like systems such as Linux and macOS. For Windows systems, the ```_findfirst()``` function can be used to achieve similar functionality.

In addition to using the ```opendir()``` function, there are other ways to check if a directory exists, such as using the ```stat()``` function or the ```access()``` function. These functions can provide more detailed information about the existence and access permissions of a directory.

## See Also:
- [opendir() documentation](https://man7.org/linux/man-pages/man3/opendir.3.html)
- [stat() documentation](https://man7.org/linux/man-pages/man2/stat.2.html)
- [access() documentation](https://man7.org/linux/man-pages/man2/access.2.html)
- [_findfirst() documentation](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/findfirst-functions?view=msvc-160)