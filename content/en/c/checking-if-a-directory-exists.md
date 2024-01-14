---
title:    "C recipe: Checking if a directory exists"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error in your code that reads "directory does not exist"? This can be frustrating, especially if you are working on a large project. Before attempting to create the directory, it is important to first check if it already exists. This will prevent any unnecessary errors and save you time in the long run.

## How To

Checking if a directory exists in C programming is a fairly simple process. Here is a code example using the `opendir` function:

```C
#include <stdio.h>
#include <stdbool.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("my_directory"); // replace "my_directory" with your directory name
    bool exists = false;
    
    if (dir) { // if opendir was successful, directory exists
        exists = true;
        closedir(dir); // close the directory
    }
    
    if (exists) {
        printf("Directory exists!\n");
    } else {
        printf("Directory does not exist.\n");
    }
    
    return 0;
}
```
Sample output:
```
Directory exists!
```

Let's break down the code. First, we need to include the necessary header files, namely `stdio.h` for input/output, `stdbool.h` for the boolean data type, and `dirent.h` for directory operations. 

Next, we use the `opendir` function to attempt to open the directory specified in the parameter. If the function is successful, the directory exists. We then set our `exists` boolean variable to true and close the directory using the `closedir` function.

Finally, we use an if statement to check the value of `exists` and print the appropriate message.

## Deep Dive

There are a few things to note when checking if a directory exists in C programming. 

Firstly, the `DIR` pointer returned by the `opendir` function can also be used to perform operations on the directory such as listing its contents using the `readdir` function.

Secondly, the `opendir` function will return `NULL` if the directory does not exist, but it can also return `NULL` for other reasons such as insufficient permissions or not enough memory. This is why we check the `exists` variable after closing the directory rather than just relying on the `opendir` function.

## See Also

Here are some additional resources on working with directories in C programming:

- [Working with Directories in C](https://www.geeksforgeeks.org/c-programming-working-with-directories/)
- [Directory Handling in C](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [C Programming Tutorial - 44: Create & Check Directories](https://www.youtube.com/watch?v=B7u5b6AFrWU)

Now that you know how to check if a directory exists in C programming, you can ensure that your code runs smoothly and avoid any pesky errors. Happy coding!