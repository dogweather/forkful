---
title:                "Creating a temporary file"
date:                  2024-01-20T17:39:45.682433-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creating a temporary file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file in C gives you a scratchpad for data processing. It's a way to store data that you need during program execution but not after it ends.

## How to:

C has functions like `tmpfile()` and `mkstemp()` to make temp files. Here's a `tmpfile()` example:

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp) {
        fputs("Write something temporary.", temp);
        // Use the file...
        rewind(temp); // Go back to the start to read what we wrote.
        
        // Let's say we want to display it:
        char buffer[100];
        while (fgets(buffer, sizeof(buffer), temp) != NULL) {
            printf("%s", buffer);
        }
        // Close and delete automatically when the program ends
        fclose(temp);
    } else {
        perror("tmpfile() failed");
    }

    return 0;
}
```
Sample output: `Write something temporary.`

## Deep Dive
Temporary files have been around since the dawn of modern operating systems. They're handy for handling large data that won't fit in memory, for inter-process communication, or for confidentiality (since they're usually deleted when the program ends).

`tmpfile()` creates a unique temporary file in binary read/write (`w+b`) mode. The file is automatically deleted when it's closed or the program ends. Just remember, since the file is opened in binary mode, if you're dealing with text, conversions for newline characters won't be handled automatically. 

If you need more control, use `mkstemp()`. It replaces template characters in your filename with a unique string, and you have to delete the file manually when you're done. 

```c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main() {
    char template[] = "/tmp/mytemp.XXXXXX";
    int fd = mkstemp(template);
    if (fd == -1) {
        perror("mkstemp() failed");
        exit(EXIT_FAILURE);
    }

    // Convert file descriptor to FILE object
    FILE *temp = fdopen(fd, "w+");
    if (temp == NULL) {
        perror("fdopen() failed");
        close(fd);
        exit(EXIT_FAILURE);
    }

    fputs("Here's to more control over temp files.", temp);
    
    // Cleanup: Close and delete manually
    fclose(temp); 
    unlink(template); // Delete the file

    return 0;
}
```
Sample output: (No explicit output, but a temp file is created and deleted)

Why not just roll your own temp file with `fopen()`? Collision risk. Remember, `tmpfile()` and `mkstemp()` ensure the filename is unique to avoid clashes.

## See Also

- C Standard Library documentation: https://en.cppreference.com/w/c/io
- GNU C Library manual for File System Interface: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html
- Secure Coding in C and C++ for handling files and data securely: https://www.securecoding.cert.org
