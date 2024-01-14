---
title:    "C recipe: Checking if a directory exists"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an essential task in many C programming projects. Whether it's for file input/output operations, creating new directories, or managing file system permissions, knowing whether a directory exists can help prevent errors and ensure proper execution of your code.

## How To

To check if a directory exists in C, we will use the `opendir()` function from the `<dirent.h>` header. This function takes in the name of the directory as a string and attempts to open it. If successful, it will return a pointer to a `DIR` structure, which represents the opened directory. If the directory does not exist, `opendir()` will return `NULL`.

Let's take a look at a simple code snippet that checks if a directory named "test" exists in the current working directory:

```C
#include <stdio.h>
#include <dirent.h>
int main(){
    // Open "test" directory
    DIR* dir = opendir("test");
    
    if (dir) {
        // Directory exists, print success message
        printf("Directory exists!\n");
        // Remember to close the directory
        closedir(dir);
    } else {
        // Directory does not exist, print error message
        printf("Directory does not exist!\n");
    }
    return 0;
}
```

When we run this code, we will get the following output:

```
Directory exists!
```

If we change the directory name to something that does not exist, for example, "fake", we will get the following output:

```
Directory does not exist!
```

It's that simple! With just a few lines of code, we are able to check if a directory exists and take appropriate actions based on the result.

## Deep Dive

For a deeper understanding of how `opendir()` works, let's take a look at its syntax:

```C
DIR * opendir(const char * name);
```

The `opendir()` function takes in the name of the directory as a string and returns a pointer to a `DIR` structure. This structure contains information about the opened directory such as the number of entries and the current position in the directory.

It's important to note that `opendir()` only checks if a file with the given name exists and not if it is a directory. So if you provide the name of a regular file, it will still return a pointer to a `DIR` structure. This is because `opendir()` is meant to be used for opening directories, not files.

## See Also

Here are some additional resources that might be helpful in understanding how to check if a directory exists in C:

- [Introduction to Directory Operations in C](https://www.geeksforgeeks.org/introduction-to-directory-in-c/)
- [C Docs - opendir function](https://www.cplusplus.com/reference/cstdio/opendir/)
- [Handling Error Conditions in C Programs](https://www.linuxjournal.com/article/6650)