---
title:                "C++ recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming, whether you are storing data for later use, creating a configuration file, or simply outputting information for the user to read. By writing a text file, you are able to store and manipulate data in a portable and readable format.

## How To

To write a text file in C++, you will need to include the `<fstream>` library. This library provides classes and functions for reading and writing files. First, you will need to create an instance of the `ofstream` class to open a file for writing. You can do this by specifying the name of the file you want to create and the mode in which you want to open it, which in this case is `ios::out` for writing.

```C++
#include <iostream>
#include <fstream>

int main() {
  // Create an instance of ofstream
  std::ofstream file("output.txt", std::ios::out);
  
  if(file.is_open()) {
    // Write to file using the insertion operator
    file << "Hello world!" << std::endl;
    file << "This is a sample text file." << std::endl;
    
    // Close the file
    file.close();
    
    // Output success message
    std::cout << "Text file successfully written." << std::endl;
  }
  else {
    std::cout << "Error opening file." << std::endl;
  }
  
  return 0;
}
```

This code will create a file named "output.txt" and write the specified text to it. If the file can be opened for writing, the program will output a success message. Otherwise, it will display an error message.

The `ofstream` class also has other methods for writing to a file, such as `put()` or `write()`, which allow you to write single characters or a sequence of characters, respectively. You can also use the `seekp()` function to set the position in the file where you want to write, and the `tellp()` function to get the current position in the file.

## Deep Dive

Text files are a type of file that stores data as a sequence of characters, with each character represented by a numeric code according to an encoding scheme. The most common encoding schemes are ASCII (American Standard Code for Information Interchange) and UTF-8 (Unicode Transformation Format 8-bit). In C++, you can use `char` and `string` types to work with characters and strings, respectively, and various functions and operators to manipulate them.

Text files contain plain text, meaning that they do not have any formatting or styling like rich text files (e.g. HTML or Word documents). This makes them versatile for storing and sharing data between different programs and platforms. However, it also means that they are not suitable for storing complex data structures like arrays or objects. In such cases, you may need to format the data in a specific way before writing it to a text file.

Additionally, it is important to ensure that the file is closed after writing to it, as leaving it open can cause issues with data not being properly written or the file becoming corrupted. You can also use the `ios::app` mode to append text to an existing file, rather than overwriting it.

## See Also

- [C++ file handling tutorial](https://www.programiz.com/cpp-programming/files-input-output)
- [UTF-8 encoding](https://www.w3schools.com/charsets/ref_utf_basic_latin.asp)
