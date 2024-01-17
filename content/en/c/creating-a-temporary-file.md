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

## What & Why?
Creating a temporary file is a common task in programming that involves creating a file that exists temporarily and is deleted after its use. Programmers use temporary files to store data or carry out operations that do not require long-term storage. They are especially useful for managing large amounts of data or performing complex tasks that cannot be done in a single run.

## How to:
To create a temporary file in C, we need to use the `tmpfile()` function from the standard input/output library `<stdio.h>`. This function dynamically creates a temporary file and returns a pointer to its `FILE` structure. Here's an example of how it can be used:

```C
#include <stdio.h>

int main() {
  FILE *temp_file = tmpfile();
  if (temp_file == NULL) {
    printf("Failed to create temporary file.\n");
    return 1;
  }
  
  fprintf(temp_file, "This is a temporary file.\n");
  fclose(temp_file);
  
  printf("Temporary file created and closed successfully.\n");
  return 0;
}
```

The above code first creates a temporary file using the `tmpfile()` function and stores its pointer in the `temp_file` variable. It then checks if the file was created successfully and prints a relevant message if not. Next, it uses the `fprintf()` function to write a string to the temporary file. Finally, it closes the file using the `fclose()` function and prints a success message.

Running this code will result in a temporary file called "file.tmp" being created in the current directory with the string "This is a temporary file." written to it. Once the program finishes executing, the temporary file is automatically deleted.

## Deep Dive:
Creating temporary files has been a common practice in programming for a long time. It allows for efficient management of data and the execution of complex tasks without clogging up the system's storage.

An alternative to using temporary files is to use memory streams, which are essentially in-memory files. However, they are limited in size and can cause memory issues when dealing with large amounts of data. Temporary files, on the other hand, can handle large data sets without any memory issues.

The `tmpfile()` function works by first checking the `TMPDIR` environment variable to determine where to store the temporary file. If the variable is not set, it uses the current directory. It then creates a unique filename using a combination of the directory path, the process ID, and a random number. This ensures that each time the function is called, a different temporary filename is created.

## See Also:
- [tmpfile() function in C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Memory streams vs Temporary files in C++](https://iq.opengenus.org/memory-streams-vs-temporary-files-cpp/)
- [Alternative ways to create temporary files in C](https://stackoverflow.com/questions/1932395/alternative-ways-to-create-temporary-files-in-c)