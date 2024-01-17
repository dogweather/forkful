---
title:                "Reading a text file"
html_title:           "C recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file in C is the process of opening and accessing the contents of a text file using the C programming language. Programmers often do this to read important data from a file or to perform operations on a file.

## How to:
To read a text file in C, we first need to include the standard input/output library, ```<stdio.h>```. Then, we declare a pointer to a file using the ```fopen()``` function, passing in the file name and the mode in which we want to open the file (e.g. "r" for reading). Once the file pointer is successfully created, we can use the ```fgets()``` function to read each line of the file until the end is reached. Finally, we close the file using the ```fclose()``` function.

Example code:

```
#include <stdio.h>

int main() {

  FILE *fp; // declaration of file pointer

  fp = fopen("example.txt", "r"); // opening the file in "r" mode

  char line[100]; // an array to store the current line

  while (fgets(line, 100, fp) != NULL) { // reads each line until end of file
    printf("%s", line); // printing out the contents of the line
  }

  fclose(fp); // closing the file

  return 0;
}
```

Sample input (example.txt):
```
Hello
This is a text file.
It contains multiple lines.
```

Output:
```
Hello
This is a text file.
It contains multiple lines.
```

## Deep Dive:
Reading text files in C has been a common practice since the early days of programming due to the widespread use of plain text files as a means of storing data. Prior to the advent of modern databases and file formats, text files were the main way of storing and sharing data.

An alternative approach to reading text files is to use command-line tools such as ```cat``` or ```grep```. However, using C to read text files allows us to process the data in a more structured manner, making it easier to perform operations on the file.

The ```fopen()``` function can open files in different modes, including "r" (read), "w" (write), and "a" (append). This allows us to not only read from but also write to or modify a text file using C.

## See Also:
- [C File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Using files in C](https://github.com/victor-iyiola/using-files-in-c) by Victor Iyiola
- [C File Handling](https://www.geeksforgeeks.org/basics-file-handling-c/) on GeeksforGeeks