---
title:    "C recipe: Reading a text file"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file may seem like a simple task, but it is an essential skill for any programmer. It allows you to access and manipulate data from external sources, making your programs more dynamic and versatile. In this post, we will explore how to read a text file in C programming.

## How To
To read a text file in C, we will use the `fopen()` function to open the file and the `fgets()` function to read the contents of the file line by line. Let's take a look at an example:

```C
#include <stdio.h>

int main() {
    FILE *file;
    char line[100];

    file = fopen("data.txt", "r"); // open the file in read mode
    if (file == NULL) { // check if the file exists
        printf("File not found.\n");
        return 1;
    }

    while (fgets(line, 100, file) != NULL) { // read each line of the file
        printf("%s", line); // print the line to the console
    }

    fclose(file); // close the file
    return 0;
}
```

In the above code, we first declare a `FILE` variable and a character array to store the lines of the file. Then, we use the `fopen()` function to open the file in read mode. If the file is successfully opened, we use a `while` loop to read each line of the file using `fgets()`. Finally, we close the file using `fclose()`.

Let's say our `data.txt` file contains the following lines:

```
Hello
World!
```

The output of our program will be:

```
Hello
World!
```

## Deep Dive
Now, let's take a deeper look at the `fopen()` and `fgets()` functions. The `fopen()` function takes two arguments - the name of the file and the mode in which it will be opened. The mode can be "r" for read, "w" for write, "a" for append, and more. It also returns a `FILE` pointer which can be used to access the file.

The `fgets()` function takes three arguments - the character array to store the line, the size of the array, and the `FILE` pointer. It reads the file line by line until it reaches the end of the file (EOF) and returns `NULL`. This allows us to use it in a `while` loop to read the entire file.

## See Also
- [C File Input/Output](https://www.programiz.com/c-programming/c-file-input-output)
- [C Standard Library - File Input/Output](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Opening and Closing Files in C](https://www.codingunit.com/c-tutorial-file-io-using-text-files)