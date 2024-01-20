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

# Checking if a Directory Exists in C++

## What & Why?
Checking if a directory exists involves determining whether a specific file path corresponds to a valid, existing directory on the file system. This is crucial in programming for error handling, preventing invalid file operations and ensuring accurate file I/O in applications.

## How to:

Here's a quick example, using the `std::filesystem` library in C++17:

```C++
#include <filesystem>

bool dirExists(const std::string& dirName_in)
{
  return std::filesystem::exists(dirName_in);
}

int main()
{
  std::cout << "Does directory exist? " << (dirExists("./your_directory") ? "Yes" : "No") << std::endl;

  return 0;
}
```

In the above example, replace 'your_directory' with the name of the directory you wish to check.

## Deep Dive

Historically, developers used OS-specific system calls to check if a directory exists. For example, they might use `stat` function in Unix-based systems or `_access` in Windows. This not only made the code less portable but also led to more verbose and error-prone code.

C++17 introduced the `std::filesystem`, making it much simpler and more portable. This library provides an abstraction for performing file system operations in a platform-independent way.

There's an alternative to `filesystem::exists` in the same library – `filesystem::is_directory`. The difference is, `exists` checks for the existence of the path (be it file or directory), while `is_directory` checks if the path is a directory.

## See Also

For more on the `std::filesystem` library, check out the official documentation: [std::filesystem](https://en.cppreference.com/w/cpp/filesystem)

To understand more about file I/O in C++, check out this guide: [C++ File I/O](https://www.cplusplus.com/doc/tutorial/files/)
For a great discussion on error handling in C++, visit this page: [C++ Exception Handling](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.htm)