---
title:    "C++ recipe: Reading a text file"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever been faced with a task of extracting data from a text file? Maybe you needed to analyze information from a log file, process a CSV file, or simply read a user input text file in your program. Whatever your reason may be, learning how to read a text file in C++ can come in handy in various programming scenarios.

## How To

To read a text file in C++, we will be using the standard library's `ifstream` class, which is used for input operations on files. First, we need to include the `fstream` header file in our program.

```
#include <fstream>
```

Next, we need to create an `ifstream` object and open the text file we want to read. The constructor of `ifstream` takes in the name of the file we want to open as a parameter.

```
ifstream inputFile("sample.txt");
```

We can also use the `open()` method to open the file, passing in the file name as a parameter.

```
inputFile.open("sample.txt");
```

Once the file is open, we can start reading from it. To read a single line from the file, we can use the `getline()` method, which takes in two parameters – the first being the `ifstream` object and the second being a string variable to store the line.

```
string line;
getline(inputFile, line);
```

To read the entire file, we can use a loop that runs until the end of the file is reached. Inside the loop, we can use `getline()` to read each line and perform any desired operations on it.

```
while (!inputFile.eof()) {
    string line;
    getline(inputFile, line);

    // Perform operations on line
}
```

It is important to close the file after we are done reading from it, using the `close()` method.

```
inputFile.close();
```

Let's take a look at a complete example of reading a text file and printing each line to the console:

```
#include <iostream>
#include <fstream>
using namespace std;

int main()
{
    ifstream inputFile("sample.txt");
    if (!inputFile) {
        cout << "Error opening file." << endl;
        return 1;
    }

    string line;
    while (!inputFile.eof()) {
        getline(inputFile, line);
        cout << line << endl;
    }

    inputFile.close();
    return 0;
}
```

### Sample Output:

```
Hello
This is a sample text file.
It contains some lines of text.
The end.
```

## Deep Dive

There are other ways of reading a text file in C++ using the `ifstream` class, such as reading a specific number of characters using the `read()` method or using the `gcount()` method to get the number of characters read. You can also use the `get()` method to read individual characters from the file.

When using `ifstream` to open a file, it is important to check if the file was successfully opened before proceeding, using the `is_open()` method. We can also check for any errors while reading from the file using the `good()` method.

To format the data from the file, we can use the `precision` and `setfill` manipulators from `<iomanip>` to specify the decimal precision and padding for floating-point values.

## See Also

- [C++ Reference - ifstream](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ Tutorial - Reading and Writing Files](https://www.programiz.com/cpp-programming/files-input-output)