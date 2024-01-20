---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Reading a Text File in C Programming Language

## What & Why?

Reading a text file in programming means retrieving and interpreting data from a text file. Programmers do this to source data, read configurations, or inventory resources like levels in a game.

## How to:

Reading a text file in C is a simple, efficient process. Below is an example of how to perform the operation:

```C
#include <stdio.h>

void main() {
    FILE *file_to_read;
    char ch;

    file_to_read = fopen("example.txt", "rt"); // Open the text file in read mode

    if (file_to_read == NULL) {
        printf("Cannot open file \n");
    }
    else {
        ch = fgetc(file_to_read); // Read the first character
        while (ch != EOF) {  // Continue reading till the end of file
            printf ("%c", ch); // Print the read character
            ch = fgetc(file_to_read); // Read the next character
        }
        fclose(file_to_read); // Close the file
    }
}
```
Running this code with an 'example.txt' file that contains the text "Hello, World!", will output:

```shell
Hello, World!
```
## Deep Dive

Historically, reading text files has been a fundamental operation in programming. It dates back to when C was introduced in the 70s. 

There are alternative ways to read a text file in C, such as using fgets() or fscanf(), but these methods may be more complex or less suited to certain tasks. 

Reading a file involves three key steps: (1) opening the file, (2) reading the contents, and (3) closing the file. This is implemented via the fopen(), fgetc() and fclose() functions respectively in the C library.

## See Also

3. [Text Files in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)