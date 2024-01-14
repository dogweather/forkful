---
title:                "C recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 
Before diving into the technicalities of writing a text file in C, let's talk about why someone would want to do it in the first place. Text files are a simple and efficient way to store and exchange data between different programs and systems. They can also be easily read and edited by humans, making them a universally accessible format for data storage. In C programming, writing a text file can be useful for tasks such as creating log files, saving user input, or exporting data for further analysis.

## How To
Now that we understand the importance of text files, let's see how we can write one using C programming language. The first step is to include the `stdio.h` header file, which contains the necessary functions for file operations. Then, we need to declare a `FILE` pointer variable and use the `fopen()` function to create a new file or open an existing one. The `fopen()` function takes two arguments - the first one is the file name, and the second one is the mode in which we want to open the file.

```C
#include <stdio.h>

int main()
{
    // Declare a FILE pointer variable
    FILE *file_ptr;

    // Open the file in write mode
    file_ptr = fopen("my_text_file.txt", "w");

    // Check if the file is opened successfully
    if (file_ptr == NULL)
    {
        printf("Error in opening file.");
        return 1;
    }

    // Write data to the file using fprintf()
    fprintf(file_ptr, "Hello there! This is a sample text file created using C programming.\n");

    // Close the file
    fclose(file_ptr);

    return 0;
}
```

In the above example, we have opened a file named "my_text_file.txt" in write mode and written a string to it using the `fprintf()` function. After writing the data, it is essential to close the file using the `fclose()` function to free up system resources.

To write multiple lines or different types of data, we can use the `fprintf()` function multiple times or use other functions such as `fputc()` and `fputs()`.

## Deep Dive
To have a better understanding of writing a text file in C, let's take a closer look at the `fopen()` function. It has six different modes: 
- `r` - read mode, used for reading from an existing file
- `w` - write mode, used for writing to a file. If the file does not exist, it will be created. If it already exists, its contents will be overwritten.
- `a` - append mode, used for adding data to the end of an existing file.
- `r+` - read and write mode, used for reading and writing to a file.
- `w+` - read and write mode, similar to `w` but does not delete the file's contents.
- `a+` - read and append mode, similar to `a` but does not delete the file's contents.

It is essential to use the correct mode when opening a file, depending on our intended use. Additionally, we can include the file's path in the file name argument for `fopen()` to create or open a file in a specific directory.

## See Also
- [File Handling in C](https://www.geeksforgeeks.org/fundamentals-of-file-io-in-c-c/)
- [Working with Files in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Introduction to Text File Handling in C](https://www.programiz.com/c-programming/c-file-input-output)