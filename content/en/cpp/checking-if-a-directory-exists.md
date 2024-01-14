---
title:    "C++ recipe: Checking if a directory exists"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why 

When working on a C++ program, it is essential to ensure that it can handle all possible scenarios, including checking if a directory exists. This can prevent unexpected errors and ensure the smooth execution of the program.

## How To 

To check if a directory exists in C++, we can use the `std::filesystem` library. This library was introduced in C++17 to handle file and directory operations. To use this library, we need to first include it in our code:

```C++
#include <filesystem>
```

Next, we can use the `std::filesystem::exists()` function to check if a directory exists. This function takes in the path of the directory as its argument and returns a boolean value indicating whether the directory exists or not. Let's look at an example:

```C++
#include <iostream>
#include <filesystem>
namespace fs = std::filesystem;
int main() {
    // Specifying the directory path
    fs::path dir_path("my_directory");
    // Checking if the directory exists
    if (fs::exists(dir_path)) {
        std::cout << "Directory exists!" << std::endl;
    } else {
        std::cout << "Directory does not exist!" << std::endl;
    }
    return 0;
}
```

Running this code will give us the following output:

```
Directory exists!
```

If we change the directory path to a non-existing one, we will get the following output:

```
Directory does not exist!
```

## Deep Dive 

Under the hood, the `std::filesystem::exists()` function uses the `stat()` system call to check for the existence of the directory. This function returns a `struct` storing information about the file or directory, including its existence. The `std::filesystem` library also provides several other functions for file and directory operations, such as creating, removing, and iterating through directories.

It is worth noting that the `std::filesystem` library is based on the Boost.Filesystem library, which has been a part of C++ for a long time. This means that if you are not using C++17, you can still use the Boost.Filesystem library to perform similar tasks.

## See Also 

- [std::filesystem reference](https://en.cppreference.com/w/cpp/filesystem)
- [Boost.Filesystem reference](https://www.boost.org/doc/libs/1_76_0/libs/filesystem/doc/reference.html) 
- [C++17 filesystem library overview](https://devblogs.microsoft.com/cppblog/beginning-with-the-c17-filesystem-library/)