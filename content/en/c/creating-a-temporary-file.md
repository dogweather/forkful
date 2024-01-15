---
title:                "Creating a temporary file"
html_title:           "C recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files in C can be useful in a variety of scenarios, from storing temporary data to performing file operations efficiently. They provide a temporary storage space that can be easily accessed and manipulated during the course of a program.

## How To
To create a temporary file in C, we can use the function `tmpfile()` from the standard library `stdio.h`. Here's an example:

```C
#include <stdio.h>

void main() {
  FILE *tempfile = tmpfile();

  // Check if file creation was successful
  if (tempfile == NULL) {
    printf("Error creating temporary file");
    return;
  }

  // Write data to temporary file
  fprintf(tempfile, "Hello World!");

  // Close the file
  fclose(tempfile);
}
```

In the above code, we first declare a file pointer `tempfile` and assign it the return value of `tmpfile()`. Then, we check if the file creation was successful by comparing the pointer to `NULL`. If it is valid, we can write data to the file using `fprintf()` just like we would for any other file. Finally, we close the file when we are done using `fclose()`.

When run, this code will create a temporary file with a random name and write "Hello World!" to it. 

## Deep Dive
Behind the scenes, the `tmpfile()` function uses the [mkstemp](https://linux.die.net/man/3/mkstemp) function from the `stdlib.h` library to create the temporary file. It creates a unique filename in the `/tmp` directory and returns a file pointer to it. Additionally, the temporary file is automatically deleted when the program ends, making it a convenient and efficient way to store temporary data.

There is also another function in the `stdio.h` library called `tmpnam()` which can be used to generate a temporary filename, but it is not recommended as it may lead to security vulnerabilities.

## See Also
- [tmpfile() documentation](https://www.man7.org/linux/man-pages/man3/stdio.3.html)
- [mkstemp() documentation](https://linux.die.net/man/3/mkstemp)
- [tmpnam() documentation](https://www.man7.org/linux/man-pages/man3/tmpnam.3.html)