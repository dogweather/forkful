---
title:                "Checking if a directory exists"
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is about confirming whether a specified folder is present on the file system. Programmers do this to avoid errors when accessing, reading, or writing to files - sort of like making sure the drawers are actually there before putting your clothes away.

## How to:

Starting with C++17, we have `std::filesystem` to make our life easier for file system operations. Here’s a code snippet to check if a directory exists:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dir_path{"./some_directory"};

    bool exists = std::filesystem::exists(dir_path);
    if(exists) {
        std::cout << "Directory exists." << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;
    }

    return 0;
}
```

Sample output (if the directory exists):
```
Directory exists.
```

Or (if the directory does not exist):
```
Directory does not exist.
```

## Deep Dive

Before C++17, we would have to rely on platform-specific API calls or third-party libraries. In the Windows API, we might have used `GetFileAttributes` and checked if the return value was `INVALID_FILE_ATTRIBUTES`. On POSIX systems, we could use the `stat()` function for similar functionality.

C++17 changed the game with `std::filesystem`. It provides cross-platform support and a high-level interface to interact with the file system. The `exists()` function is the straightforward way to check for directory existence, but you can also use `is_directory()` if you not only want to confirm existence but also that the path points to a directory and not a file.

For alternative methods, consider the `std::filesystem::status_known()` and `std::filesystem::file_status` functions to handle cases where file permissions or other issues might affect your ability to determine if a directory exists.

## See Also

Explore more on file system operations in C++:

- [std::filesystem documentation](https://en.cppreference.com/w/cpp/filesystem)
- For historical context and differences between versions, see [C++ version history](https://en.cppreference.com/w/cpp/compiler_support)