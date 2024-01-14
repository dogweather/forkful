---
title:                "C++ recipe: Checking if a directory exists"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
"Checking if a directory exists" may not sound like the most exciting coding task, but it is an important one. As developers, we want to make sure our code can handle any situation, including when a directory may or may not exist. By checking for the existence of a directory, we can prevent errors and ensure our code runs smoothly.

## How To
To check if a directory exists in C++, we can use the `std::filesystem` library. First, we need to include this library in our code:

```
#include <filesystem>
```
Next, we can use the `exists()` function from the `std::filesystem` library to check for the existence of a directory. Here's an example:

```
if (std::filesystem::exists("/path/to/directory")) {
  std::cout << "Directory exists!";
} else {
  std::cout << "Directory does not exist.";
}
```
The `exists()` function takes in a path as its parameter and returns a `bool` value. If the directory exists, it will return `true`, otherwise it will return `false`.

We can also use the `is_directory()` function to further check if the file path given belongs to a directory or a regular file. Here's an example:

```
if (std::filesystem::is_directory("/path/to/file.txt")) {
  std::cout << "This is a directory!";
} else {
  std::cout << "This is a regular file.";
}
```
You can also check for the existence of a relative directory using the `current_path()` function. This will return the current working directory, which we can then use to check for the existence of a subdirectory. Here's an example:

```
if (std::filesystem::exists(std::filesystem::current_path() / "subdirectory")) {
  std::cout << "Subdirectory exists within current working directory!";
} else {
  std::cout << "Subdirectory does not exist within current working directory.";
}
```

## Deep Dive
Now, let's dive a little deeper into how the `std::filesystem` library actually checks for the existence of a directory. Under the hood, this library uses the underlying file system to perform the check. This means it will look for the directory in the file system and return the appropriate result.

However, keep in mind that there can be some issues with using this library, such as different file systems having different limitations and functionalities. So it's important to understand how this library works and to test your code on different file systems to ensure it works as expected.

## See Also
- std::filesystem reference (https://en.cppreference.com/w/cpp/filesystem)
- Filesystems in C++17 (https://devblogs.microsoft.com/cppblog/filesystem-features-in-c17/)
- C++ Tutorial: Filesystem and error handling (https://www.learncpp.com/cpp-tutorial/185-file-io-with-native-c/)
- Github Repository for code examples (https://github.com/yourusername/your-repo-name)