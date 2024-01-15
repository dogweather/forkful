---
title:                "Checking if a directory exists"
html_title:           "C++ recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an essential task in programming, especially when dealing with file manipulation or writing scripts. By ensuring the existence of a directory, we can avoid runtime errors and handle any unexpected scenarios effectively.

## How To

To check if a directory exists in C++, we use the `std::filesystem` library, which was introduced in C++17. This library provides an easy and platform-independent way of working with files and directories.

First, we need to include the `<filesystem>` header in our program. Then, we can check if a directory exists using the `exists()` function from the `std::filesystem` namespace.

```C++
#include <filesystem>

// Path of the directory to be checked
std::filesystem::path dirPath = "directory_name";

// Check if the directory exists
if (std::filesystem::exists(dirPath)) {
    std::cout << "Directory exists!";
} else {
    std::cout << "Directory does not exist!";
}
```

The `exists()` function returns a boolean value - `true` if the directory exists and `false` if it doesn't. We can also use the `is_directory()` function to check if the given path is a directory, but not necessarily if it exists.

```C++
// Check if the given path is a directory
if (std::filesystem::is_directory(dirPath)) {
    // Directory exists and it is indeed a directory
}
```

The `exists()` function works for both relative and absolute paths. We can also use `path::operator/()` to create a path by appending a directory name to a root path.

```C++
// Create a path with the directory name appended to the root path
std::filesystem::path dirPath = "/home/users/";
dirPath /= "directory_name";

if (std::filesystem::exists(dirPath)) {
    // Directory exists!
}
```

If we want to perform some other operations if the directory does not exist, we can use the `copy()` function along with `error_code` to handle any potential errors.

```C++
std::filesystem::error_code errorCode;

// Copy a file from directory if it exists
std::filesystem::copy("directory_name/file.txt", "new_directory/", errorCode);

if (errorCode) {
    // An error occurred while copying, handle it here
} else {
    // File copied successfully
}
```

## Deep Dive

Under the hood, the `exists()` function uses the `stat()` system call, which checks the existence and status of a file or directory. In case of an absolute path, the `stat()` call first checks if the specified file or directory exists, and then if it is indeed a directory. For relative paths, it works in a similar manner but relative to the current working directory.

Moreover, the `exists()` function also follows any symbolic links or junction points and returns `true` if any of the intermediaries leading to the final path exists. This behavior can be changed by using the `std::filesystem::file_status` function, which provides additional options such as `none`, `regular`, or `symlink`.

## See Also

- [C++ filesystem reference](https://en.cppreference.com/w/cpp/filesystem)
- [Standard C++ Library in C++17](https://en.wikipedia.org/wiki/C%2B%2B17#Standard_library)
- [File handling in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)