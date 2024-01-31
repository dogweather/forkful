---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file involves saving data in a readable format on the filesystem. Programmers do it to persist information, like settings, logs, or user-generated content, for later retrieval and processing.

## How to:
Writing to a text file in C is straightforward with the `stdio.h` library functions: `fopen()`, `fprintf()`, and `fclose()`. Check out this simple example:

```C
#include <stdio.h>

int main() {
    FILE *filePointer = fopen("example.txt", "w"); // Open file in write mode
    if (filePointer == NULL) {
        printf("Error opening file.\n");
        return 1;
    }
    
    fprintf(filePointer, "Hello, world!\n"); // Write to file
    fprintf(filePointer, "Writing to files in C is simple.\n");
    
    fclose(filePointer); // Close file
    return 0;
}
```
Sample output in `example.txt`:
```
Hello, world!
Writing to files in C is simple.
```

## Deep Dive
Since its inception with C's ancestor languages, file I/O has been crucial for programs. Alternatives to `stdio.h` include system-level calls like `open()`, `write()`, and `close()` from `sys/file.h`, which offer more control but are more complex. When using `stdio.h`, buffering can affect performance, so for large files or frequent writes, function `fflush()` might be needed.

## See Also
For more on file operations in C:
- C Standard Library Documentation: https://en.cppreference.com/w/c/io
- C File I/O Tutorial: http://www.cplusplus.com/doc/tutorial/files/
- Managing File I/O: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html
