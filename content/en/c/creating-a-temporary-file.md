---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file in C is the process of generating short-term storage during a program's execution. They're handy for storing data you only need while your program runs, like dumping large calculations, intermediate processing, buffering, or as a handy safety net to avoid data loss during crash scenarios.

## How to:

Creating a temporary file in C is relatively straightforward, thanks to the `tmpfile()` function included in the `stdio.h` file. Here is an example:

```C
#include <stdio.h>

int main() {
    FILE *tmpf = tmpfile();
    fputs("Hello, World!", tmpf);
    rewind(tmpf); 
    char buf[14];
    fgets(buf, sizeof(buf), tmpf);
    puts(buf); 
    return 0;
} 
```

When you run the above program, it will display "Hello, World!" as output. We used the `fputs()` and `fgets()` functions to write to and read from the temporary file respectively. The `rewind()` function helps us reset the file pointer to the start to read from the beginning.

## Deep Dive

Temporary files have been a part of C since its inception, crucial tools for managing computer resources efficiently. The `tmpfile()` function essentially creates a unique file in the default directory for temporary files (usually '/tmp' or '/var/tmp' on Unix-like systems). The open file is then automatically set to be deleted when it's closed, either by an explicit `fclose()` call or when program terminates.

Alternatives to `tmpfile()` include `mkstemp()`, which allows for more control over the file’s name and location, and `tmpnam()` or `tempnam()`, which generate a name for a temporary file but they are less safe to use than `tmpfile()` because of possible race condition issues.

Annotations regarding the implementation of `tmpfile()`: underneath, this function is performing a few tasks. First, it constructs a unique filename. Then, it opens the file in binary read/write mode (`wb+`), and finally sets it to be deleted when closed.

## See Also:

- Detailed information about `tmpfile()`: [http://www.cplusplus.com/reference/cstdio/tmpfile](http://www.cplusplus.com/reference/cstdio/tmpfile)
- More about `mkstemp()`: [https://linux.die.net/man/3/mkstemp](https://linux.die.net/man/3/mkstemp)
- About `tmpnam()` and its risks: [https://en.cppreference.com/w/c/io/tmpnam](https://en.cppreference.com/w/c/io/tmpnam)
- For help understanding file modes like `wb+`: [https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)