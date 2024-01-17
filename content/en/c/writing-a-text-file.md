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

## What & Why?

Writing a text file means creating a file that contains readable text. Programmers often do this as a way to store data or code that can be easily accessed and manipulated.

## How to:

To write a text file in C, you can use the `fprintf()` function. This function takes in a file pointer, a format string, and any necessary arguments. Here's an example:

```C
// Opening the file with write mode
FILE *file = fopen("sample.txt", "w");

// Writing `Hello, world!` to the file
fprintf(file, "Hello, world!");

// Closing the file
fclose(file);
```

This will create a file named `sample.txt` with the text `Hello, world!` written in it.

## Deep Dive:

Writing text files has been a common practice in programming for many years. Before the rise of databases and other data storage methods, text files were one of the main ways to store and access data. They also have the advantage of being human-readable, making them useful for storing code or configurations.

There are several alternatives to writing text files, such as using databases, JSON files, or CSV files. Each method has its own advantages and use cases, but writing text files remains a simple and efficient way to store and retrieve data.

When writing a text file, it's important to pay attention to the file format and encoding. Different operating systems and programs may use different standards, so it's crucial to ensure compatibility when sharing or accessing the file.

## See Also:

- [fopen() function reference](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Working with files in C](https://www.programiz.com/c-programming/c-file-input-output)