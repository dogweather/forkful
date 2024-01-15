---
title:                "Writing a text file"
html_title:           "C recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file in C allows you to store data permanently and access it for future use. This can be useful in various scenarios, such as saving user preferences or keeping track of a program's progress.

## How To

To write a text file, you will need to follow these steps:

```C
// include necessary libraries
#include <stdio.h>

int main() {
  // declare a file pointer
  FILE *fp;
  
  // open the file in write mode
  fp = fopen("test.txt", "w");
  
  // write data to the file using fprintf()
  fprintf(fp, "This is a sample text file.");
  
  // close the file
  fclose(fp);
  
  return 0;
}
```

This code will create a file named "test.txt" in the same directory as your program and write the given text to it. To write more data, you can use the same `fprintf()` function with multiple lines of text or use a loop to write data from a variable.

To make sure the data is written successfully, you can check the return value of `fprintf()` - it will return the number of characters written to the file.

## Deep Dive

When writing a text file in C, there are a few important things to keep in mind:

- Make sure to open the file in the correct mode - `w` for write mode, `r` for read mode, `a` for append mode, and `w+` for read/write mode.
- Use appropriate formatting in `fprintf()` to control the structure and readability of your text file.
- Remember to close the file after writing to it - this will ensure that the data is written successfully and any changes are saved.

Additionally, there are various other functions you can use to manipulate text files, such as `fscanf()` for reading data from a file and `rename()` for renaming a file.

## See Also

- A Beginner's Guide to File Input and Output in C - [https://www.programiz.com/c-programming/c-file-input-output](https://www.programiz.com/c-programming/c-file-input-output)
- C Tutorial - File I/O - [https://www.tutorialspoint.com/cprogramming/c_file_io.htm](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- C File Handling - [https://www.geeksforgeeks.org/file-handling-c-classes/](https://www.geeksforgeeks.org/file-handling-c-classes/)