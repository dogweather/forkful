---
title:                "C recipe: Writing a text file"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Why Write a Text File in C Programming

Text files are an essential part of any programming language and can serve many purposes. In C programming, text files are commonly used to store and retrieve data. This allows for persistence of data even after the program has ended and can be useful for creating databases, configuration files, and more.

# How To Write a Text File in C Programming

To create a text file in C, we first need to declare a file pointer and open the file using the `fopen()` function. We need to specify the file name and access mode, such as "w" for writing. Then, we can use the `fprintf()` function to write data to the file. For example:

```
#include <stdio.h>

int main() {
   FILE* fp = fopen("textfile.txt", "w");
   fprintf(fp, "Hello world!");
   fclose(fp);
   return 0;
}
```

This code will create a text file named `textfile.txt` and write the string "Hello world!" to it. We can also use the `fputs()` function to write data to a text file. Here's an example:

```
#include <stdio.h>

int main() {
   FILE* fp = fopen("textfile.txt", "w");
   fputs("Hello world!", fp);
   fclose(fp);
   return 0;
}
```

Both of these examples will produce the same output. However, the `fprintf()` function allows for more control over the formatting of the data being written.

We can also read data from a text file using the `fscanf()` and `fgets()` functions. These functions allow us to retrieve data from a text file and use it in our program.

# Deep Dive into Text File Writing in C Programming

When writing a text file in C, it is important to remember to close the file once we are done using it. This ensures that all the data is saved and the file is not left open, which can cause issues.

Another thing to keep in mind is the use of escape characters. These are special characters that have a specific meaning in C programming. For example, the `\n` escape character represents a new line and can be used to format our text file.

We can also specify the data type when using `fprintf()` or `fscanf()` to write or read data from a text file. This can be useful when we are storing different types of data in the same file, such as integers and strings.

# See Also

- [C Programming Tutorial - File input/output](https://www.programiz.com/c-programming/c-file-input-output)
- [Writing Files in C](https://www.tutorialspoint.com/c_standard_library/c_function_fputs.htm)
- [C File I/O](https://www.geeksforgeeks.org/basics-file-handling-c/)