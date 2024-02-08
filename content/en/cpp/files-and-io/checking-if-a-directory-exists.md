---
title:                "Checking if a directory exists"
date:                  2024-02-03T19:02:35.355748-07:00
model:                 gpt-4-0125-preview
simple_title:         "Checking if a directory exists"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is about determining the presence of a directory at a specified path before performing operations like reading from or writing to files within it. Programmers do it to avoid errors related to file operations, ensuring a smoother and more reliable execution of file handling tasks in their applications.

## How to:
In modern C++ (C++17 and beyond), you can use the filesystem library to check if a directory exists. It provides a straightforward and standardized way to perform filesystem operations, including checking for the existence of a directory.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
Sample output if the directory exists:
```
The directory exists.
```

Sample output if the directory does not exist:
```
The directory does not exist.
```

For projects that are not yet using C++17 or for additional features, the Boost Filesystem library is a popular third-party choice that offers similar functionality. 

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
Using Boost Filesystem, the output would be identical to the C++17 filesystem example, depending on the existence of the directory at the specified path.
