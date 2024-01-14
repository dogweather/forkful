---
title:                "C recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading and manipulating data from text files is a crucial aspect of programming in any language, including C. Understanding how to read text files not only expands your programming knowledge, but it also allows you to work with different file types and store and retrieve data more efficiently.

## How To

Reading a text file in C is a simple process that involves a few steps. Let's take a look at an example:

```
#include <stdio.h>

int main() {
    char c;
    FILE *fp;

    fp = fopen("./example.txt", "r"); // open file in read mode
    if (fp == NULL) { // check if file was opened successfully
        printf("Error opening file.\n");
        return -1;
    }

    // read and print each character until the end of file
    while ((c = fgetc(fp)) != EOF) {
        printf("%c", c);
    }

    fclose(fp); // close file
    return 0;
}
```

In this example, we first include the `stdio.h` library, which allows us to work with files in C. Then, we declare a character variable `c` and a file pointer `fp` which will be used to open and read the text file. Next, we use the `fopen()` function to open the file, which takes in two arguments - the file name and the mode in which we want to open it. In this case, we use `"r"` which stands for read mode. We also check if the file was opened successfully and if not, we print an error message and exit the program.

Once the file is successfully opened, we use a `while` loop to read and print each character one by one using the `fgetc()` function. This function returns the next character in the file, and the loop continues until the `EOF` (end of file) character is encountered. Finally, we close the file using the `fclose()` function to free up any resources being used.

If we have a text file named `example.txt` with the following contents:

```
Hello world!
This is a test file.
```

The output of our program would be:

```
Hello world!
This is a test file.
```

## Deep Dive

Reading a text file may seem like a simple task, but there are a few important things to keep in mind when working with files in C. One of these is error handling. In the above example, we checked if the file was opened successfully using `fp == NULL`. This is necessary to prevent the program from crashing if the file does not exist or if there are any other errors.

Another important aspect is closing the file after we are done using it. This is done using the `fclose()` function, which not only frees up resources but also ensures that any changes made to the file are saved.

It is also important to know that the `fgetc()` function reads one character at a time, so if we want to read the entire file line by line, we can use the `fgets()` function instead.

Additionally, we can use other modes besides `"r"` to open files, such as `"w"` for write mode, and `"a"` for append mode. These modes can be used to create new files, modify existing ones, or add new content to the end of a file.

Overall, understanding how to read text files in C opens up many possibilities for storing and retrieving data, and it is a valuable skill for any programmer.

## See Also

- [Reading and Writing Files in C](https://www.programiz.com/c-programming/c-file-input-output)
- [File Handling in C](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [C File Input/Output](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)