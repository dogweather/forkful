---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating temporary files in C++ means generating files that hold data temporarily during your program’s execution. Useful for things like caching heavy computations or storing information between multiple application runs.

## How To:

Luckily, the `<filesystem>` library in C++ contains functions for creating temporary files. Here’s a quick example:

```C++
#include <filesystem>
#include <iostream>

int main() {
    std::filesystem::path tmpPath = std::filesystem::temp_directory_path();
    tmpPath /= "tempFileXXXXXX";   // pattern for the temporary file name

    char tmpName[] = "tempFileXXXXXX";
    int fileDescriptor = mkstemp(tmpName);

    std::cout << "Temporary file created: " << tmpName << std::endl;

    close(fileDescriptor);  // Don't forget to close the file descriptor!
    return 0;
}
```

This will create a file with a unique name in your system's temp directory.

## Deep Dive 

Historically, creating temporary files in C++ was not as easy. Before introducing the `<filesystem>` library, developers often had to write OS-specific code or employ third-party libraries. Having this functionality in standard C++ significantly simplifies things.

As an alternative, one might consider creating temporary files in RAM using a RAM disk. This is usually faster but consumes more memory. 

In our code, `mkstemp` generates a unique temporary file, replacing 'XXXXXX' in the file name with a unique string to ensure the file does not exist. The `fileDescriptor` returned can be used to interact with the file.

## See Also

For further knowledge:
- [C++ Filesystem library](https://en.cppreference.com/w/cpp/filesystem)