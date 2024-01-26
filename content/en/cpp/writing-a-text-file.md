---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file in C++ means creating or modifying a file to store textual data. Programmers do it to persist data like configurations, logs, or user-generated content.

## How to:
Below is a simple C++ program that creates a text file and writes "Hello, World!" to it.

```c++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("hello.txt");

    if (outfile.is_open()) {
        outfile << "Hello, World!";
        outfile.close();
        std::cout << "File written successfully\n";
    } else {
        std::cout << "Error opening file\n";
    }

    return 0;
}
```
Sample output:
```
File written successfully
```

## Deep Dive
In C++, files are handled by the `<fstream>` header, which provides `std::ofstream` for writing, `std::ifstream` for reading, and `std::fstream` for both. Historically, file I/O in C++ evolved from the C `FILE` structure and related functions. Alternatives to `fstream` include platform-specific APIs, third-party libraries, or modern C++ proposals like filesystem library enhancements. When writing files, handle errors and ensure resources are released properly, typically using RAII patterns available in modern C++.

## See Also
- C++ File I/O: http://www.cplusplus.com/doc/tutorial/files/
- C++ Reference (ofstream): https://en.cppreference.com/w/cpp/io/basic_ofstream
- C++ Filesystem Library: https://en.cppreference.com/w/cpp/filesystem
