---
title:    "C++ recipe: Writing a text file"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you may be wondering why you would need to write a text file using C++. Text files are a simple and efficient way to store and manipulate data. They can be used for a variety of purposes such as saving user preferences, storing game scores, or even creating a log file for debugging.

## How To
To write a text file in C++, you will need to use the `<fstream>` library. This library provides classes and functions that allow you to easily create, open, and write to a text file. Let's take a look at an example:

```C++
#include <iostream>
#include <fstream>

int main() {
    // Create an output file stream
    std::ofstream file("my_text_file.txt");

    // Write a string to the file
    file << "Hello, world!";

    // Close the file
    file.close();

    // Output success message
    std::cout << "Text file created successfully." << std::endl;

    return 0;
}
```
Running this code will create a new text file called "my_text_file.txt" and write the string "Hello, world!" to it. You can also use the `open()` and `write()` functions to open and write to a file separately.

## Deep Dive
When writing a text file, it's important to understand the different modes in which you can open the file. The three modes are: `ios::out` for writing to the file, `ios::in` for reading from the file, and `ios::app` for appending to the end of the file. By default, if no mode is specified, the file will be opened in `ios::out` mode.

You can also specify the format in which data will be written to the file. For example, if you want to write a floating point number with 2 decimal places, you can use the `setprecision()` function from the `<iomanip>` library. Additionally, you can use the `<<` operator to write various data types to the file, such as integers, characters, or strings.

It's important to properly handle errors when writing to a file. You can use the `fail()` function to check if the operation was successful and the `clear()` function to reset any error flags.

## See Also
If you want to learn more about writing text files in C++, here are some helpful resources:

- [Working with Files in C++ - GeeksforGeeks](https://www.geeksforgeeks.org/working-with-files-in-c/)
- [File Handling in C++ - Cplusplus.com](http://www.cplusplus.com/doc/tutorial/files/)
- [Input/output with files - CppReference.com](https://en.cppreference.com/w/cpp/io)

Start using text files in your C++ programs to make data manipulation easier and more organized. Happy coding!