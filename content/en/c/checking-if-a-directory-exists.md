---
title:                "Checking if a directory exists"
date:                  2024-01-19
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is about confirming whether a specific folder is present in the file system. Programmers do it to avoid errors like trying to access or create files in a non-existent directory, which could crash a program or lead to data loss.

## How to:

We'll use the `stat` function from the `sys/stat.h` header to check for directory existence in C. Here's a simple code example:

```C
#include <stdio.h>
#include <sys/stat.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0; // Directory does not exist or error occurred
    }
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path_to_check = "/path/to/directory";
    if (directory_exists(path_to_check)) {
        printf("Directory exists!\n");
    } else {
        printf("Directory does not exist.\n");
    }
    return 0;
}
```

Sample output if the directory exists:

```
Directory exists!
```

Or, if it doesn't:

```
Directory does not exist.
```

## Deep Dive

The `stat` function has been around since Unix's early days, part of POSIX specifications. It grabs info about the file or directory at the given path, and that info gets stored in a `struct stat`. Specifically, we check the `st_mode` field to determine if the path points to a directory.

Alternatives to `stat` include `access` or `fstatat` in C. In Linux, you can also tap into higher-level APIs like `g_file_test` from the GLib library.

For implementation details, keep these in mind:

- `stat` can fail not just when the directory doesn't exist but also due to permissions issues or a bad path. Error checking is essential.
- Symbolic links need special handling; `lstat` is used instead of `stat` if you're dealing with them.
- Performance may vary. If you're checking multiple properties or doing several checks, there might be more efficient paths.

## See Also

- POSIX `stat` documentation: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
- GLib File Utilities: [https://docs.gtk.org/glib/func.file_test.html](https://docs.gtk.org/glib/func.file_test.html)
