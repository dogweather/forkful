---
title:                "Writing a text file"
html_title:           "C++ recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Text files are used for storing and retrieving data in a plain text format. Unlike other file formats, they can be easily read and edited by both humans and computers. Programmers often use text files to store configurations, data for their applications, and other important information.

## How to:

To write a text file in C++, you will need to use the standard library function `ofstream`, which stands for output file stream. Here is a simple code example:

```C++
#include <iostream>
#include <fstream>

int main() {
    // Open a text file named "example.txt" to write to
    std::ofstream file("example.txt");

    // Write a line of text to the file
    file << "This is an example text file.";

    // Close the file
    file.close();
    
    return 0;
}
```

And here is the output in the "example.txt" file:

```
This is an example text file.
```

## Deep Dive:

Historically, text files have been a common way of storing and sharing data since the early days of computing. Before the introduction of fancy database systems and complex file formats, a plain text file was often the go-to choice. It also played a significant role in the development of the internet as it allowed for easy sharing of information across different systems.

While there are other ways to store and organize data, using text files has its benefits. They are lightweight, easy to read and write, and do not require any special software to open. In fact, you can even use a simple text editor to view and modify a text file.

However, as with everything else in programming, there are alternatives to writing a text file. You can also use binary files, which have a more specific format and are optimized for storage and retrieval of data. Additionally, there are libraries and frameworks available that can make the process of writing and reading text files easier for the programmer.

The implementation details of writing a text file may vary depending on the operating system and the specific programming language being used. However, the basic principles remain the same. You open a file, write or read from it, and then close it. It is essential to always close the file after you are finished with it to prevent any data loss or corruption.

## See Also:

- [C++ ofstream documentation](https://www.cplusplus.com/reference/fstream/ofstream/)