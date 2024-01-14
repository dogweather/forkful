---
title:    "C++ recipe: Writing a text file"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Many computer programs require the use of external files to store and manage data. Text files are a popular choice due to their simplicity and compatibility across different operating systems. In this blog post, we will delve into the world of writing text files in C++ and explore the various functionalities it offers.

## How To

Writing a text file in C++ involves a series of steps that begin with creating an instance of the `ofstream` class and providing it with the desired file name. We can then use the `open()` function to open the file in either input or output mode, depending on our requirements.

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main()
{
    // creating an instance of the ofstream class
    ofstream file;

    // opening the file in output mode
    file.open("sample.txt");

    // writing data to the file
    file << "Hello, world!";

    // closing the file
    file.close();

    // displaying success message
    cout << "File created and data written successfully.";

    return 0;
}
```

The above code creates a new text file named "sample.txt" and writes the string "Hello, world!" into it. We use the `<<` operator to transfer the data into the file. Once we are finished writing, it is important to close the file using the `close()` function to save our changes and free up any resources being used by the `ofstream` instance.

### Sample Output

```
File created and data written successfully.
```

There are also other useful functions provided by the `ofstream` class that can be used for more advanced operations such as appending data to an existing file or checking if a file exists before opening it.

## Deep Dive

When writing to a text file, one must be aware of the different modes in which the file can be accessed. These modes determine the actions that can be performed on the file, such as reading, writing, or appending data. The most commonly used modes are "out" (output) and "app" (append).

```C++
// opening the file in output mode
file.open("sample.txt", ios::out);

// opening the file in append mode
file.open("sample.txt", ios::app);
```

Another important aspect to consider is the formatting of the data being written to the file. By default, data is written to the file in its raw form, without any formatting. For example, if we write multiple strings to a file without any spacing or new line characters, they will all be displayed as one continuous string when the file is opened.

To overcome this, we can use the `setw()` function from the `<iomanip>` library to set the width of the data being written, and `endl` to add a new line after each piece of data.

```C++
// including the <iomanip> library for setw()
#include <iomanip>

// setting the width of data to be written to 10 characters
file << setw(10) << "Hello" << endl;
file << setw(10) << "World" << endl;

// output in file:
// Hello
// World
```

## See Also

- [C++ Documentation on Streams and Files](https://en.cppreference.com/w/cpp/io)
- [GeeksforGeeks: Reading and Writing Files in C++](https://www.geeksforgeeks.org/reading-writing-text-files-c/)
- [Tutorialspoint: File Input/Output in C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)