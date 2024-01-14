---
title:    "C++ recipe: Checking if a directory exists"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Checking if a directory exists is an essential task in C++ programming. It ensures that your applications can handle different scenarios and prevent potential errors.

## How To
To check if a directory exists in C++, you can use the `std::filesystem::exists()` function from the `<filesystem>` library. Here is an example code:

```
#include <iostream>
#include <filesystem>

int main() {
  // Define the directory path
  std::string path = "C:/Users/User/Documents";

  // Check if the path exists
  if (std::filesystem::exists(path)) {
    std::cout << "The directory exists.";
  } else {
    std::cout << "The directory does not exist or access is denied.";
  }

  return 0;
}
```

The `std::filesystem::exists()` function returns a `bool` value, `true` if the directory exists and `false` if it does not. You can also use `std::filesystem::is_directory()` function to check if the given path is a directory or not.

## Deep Dive
The `std::filesystem` library was introduced in C++17, making directory and file operations much easier and more efficient. The `exists()` function uses OS-specific APIs to check the existence of a directory and returns the result. It also allows us to check for file existence and other directory operations such as creating, renaming, and removing directories.

Some important things to note while using `std::filesystem` library:
- The `path` parameter in the functions can be of type `std::string`, `const char*` or `std::string_view`.
- The library is portable, meaning it can be used on different OS platforms without any modifications.
- To check if a given path exists, the program needs permission to execute that operation.
- You can also use `std::filesystem::current_path()` to get the current working directory.

## See Also
- [std::filesystem reference](https://en.cppreference.com/w/cpp/filesystem)
- [C++17 - std::filesystem](https://docs.microsoft.com/en-us/cpp/standard-library/filesystem?view=msvc-160)
- [Checking if a File or Directory Exists using C++17](https://thispointer.com/check-if-a-file-or-directory-exists-using-c17-filesystem-library/)