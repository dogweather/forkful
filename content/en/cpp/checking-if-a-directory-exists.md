---
title:                "C++ recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When writing a C++ program, it is important to ensure that your code can handle all possible scenarios. One common scenario is checking if a directory exists. This is crucial because it prevents your program from crashing or running into errors if a directory does not exist.

## How To

To check if a directory exists in C++, we can use the `std::filesystem::exists` function from the `<filesystem>` library. This function returns a `bool` value indicating whether the directory exists or not. We can then use an `if` statement to perform certain actions based on the result.

Let's take a look at an example:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string directory = "my_directory";

    // Check if directory exists
    if (std::filesystem::exists(directory)) {
        std::cout << "Directory exists!" << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;
    }

    return 0;
}
```

#### Sample Output
```
Directory does not exist.
```

In this example, we are using the `std::filesystem::exists` function to check if a directory named "my_directory" exists in our system. Since it does not exist, the `else` statement will be executed and we will see the output "Directory does not exist."

If we were to change the `directory` variable to an existing directory name, such as "my_folder," the output would be "Directory exists!".

## Deep Dive

Behind the scenes, the `std::filesystem::exists` function uses the `stat()` system call to check if the directory exists. This function returns a `struct stat` object containing information about the file or directory, including its type and permissions. The `st_mode` field of this object is then checked to determine if it is a directory or a file.

One important aspect to note is that the `std::filesystem::exists` function can also check for symbolic links. If you want to check for the existence of a regular file instead of a directory, you can use the `std::filesystem::is_regular_file` function.

## See Also

- [C++ File System Library Reference](https://en.cppreference.com/w/cpp/filesystem)
- [C++ File System Tutorial](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [C++ stat() system call reference](https://man7.org/linux/man-pages/man2/stat.2.html)