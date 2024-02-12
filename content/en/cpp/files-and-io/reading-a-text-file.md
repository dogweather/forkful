---
title:                "Reading a text file"
aliases: - /en/cpp/reading-a-text-file.md
date:                  2024-01-20T17:54:03.099492-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is about pulling data from a file on disk into your program to work with it. Programmers do this to handle input, configuration, or data storage without hard-coding stuff into the program.

## How to:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt");
    std::string line;

    if (file.is_open()) {
        while (getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Unable to open file";
    }
    
    return 0;
}
```
If `example.txt` contains:
```
Hello, world!
This is a test file.
```
The output will be:
```
Hello, world!
This is a test file.
```

## Deep Dive

Back in the day, data storage and retrieval were pretty cumbersome. With the advent of higher-level programming languages, operations like reading from a text file became simpler. C++ offers several ways to read from files, leveraging input/output streams provided by the standard library.

Alternatives to <fstream> for file I/O include using older C functions (like fopen, fgets, etc.), operating system-specific APIs, or other libraries that abstract away some of the lower-level details.

When we talk about implementation details, it's essential to know that `std::ifstream` is a class that handles input file streams. The key functions involved are `is_open()` to check if the file stream was successfully opened, `getline()` to read the file line by line, and `close()` to close the file stream. It's crucial to manage file resources correctly to avoid leaks or data corruption. Luckily, modern C++ (C++11 and later) includes features like RAII, which can handle resource management more safely through object lifetimes.

## See Also

- [cppreference.com - Input/output library](https://en.cppreference.com/w/cpp/io)
- Stack Overflow: [How can I read and parse CSV files in C++?](https://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c)
