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

## What & Why?

In programming, checking if a directory exists is the act of verifying 
whether a certain directory is present in a given location on a computer. 
This is important for programmers as it ensures that the program can 
properly access and manipulate files within that directory. 

## How to:

To check if a directory exists in C++, we can use the `std::filesystem::exists()` 
function from the C++17 standard library. This function takes in a `std::filesystem::path` 
object as a parameter, which represents the path to the desired directory. 
Here's an example of how we can use this function:

```C++
#include <iostream>
#include <filesystem>

int main() {
  std::filesystem::path directory = "C:/Example/Directory";
  if (std::filesystem::exists(directory)) {
    std::cout << "Directory exists!";
  } else {
    std::cout << "Directory does not exist!";
  }
  return 0;
}
```

Running this code would output `Directory exists!` if the specified directory exists 
or `Directory does not exist!` if it does not.

## Deep Dive

Checking if a directory exists has been a necessary task for programmers since 
the creation of the first computer operating systems. In earlier versions of 
C++, this was done using a system-specific method or by using external libraries. 
However, with the addition of the `std::filesystem` library in C++17, this task 
has become much simpler and more efficient.

As an alternative, some programmers may use commands like `ls` or `dir` to 
list the contents of a directory and check if the desired one is present. 
While this approach may work, it is not as reliable as using the `std::filesystem::exists()` 
function as it only checks if the specified directory exists and does not 
take into account other factors that may affect its accessibility.

When implementing the `std::filesystem::exists()` function, the C++ standard 
provides a platform-independent way of accessing and checking file and directory information. 
This is achieved by using the `std::filesystem::path` class, which stores 
the path to a file or directory as a string, and the `std::error_code` class, 
which is used to handle any errors that may occur during the process.

## See Also

To learn more about the `std::filesystem` library and its functionalities, 
you can refer to the official C++ reference guide 
[here](https://en.cppreference.com/w/cpp/filesystem). Additionally, you can 
explore the `std::filesystem` header file to discover other useful functions 
and classes for handling files and directories in C++.