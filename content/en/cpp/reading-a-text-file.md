---
title:    "C++ recipe: Reading a text file"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Reading and manipulating text files is an important aspect of programming in any programming language. In C++, it is especially useful for tasks such as data analysis, parsing raw data, and creating user-friendly interfaces. Learning how to read and write text files in C++ can greatly enhance your programming skills and enable you to tackle a wider range of projects.

## How To

To read a text file in C++, we need to use the `fstream` header file. This file provides the necessary functions for opening, reading, and closing text files. Let's take a look at a simple example:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    // Create an object of ifstream (input file stream) type
    ifstream file;
    // Open the text file
    file.open("sample.txt");

    // Check if the file is open and readable
    if(!file)
    {
        cout << "Error opening file.";
        return 0;
    }

    // Read the file line by line
    string line;
    while(getline(file, line))
    {
        // Output each line to the console
        cout << line << endl;
    }

    // Close the file
    file.close();
    
    return 0;
}
```

In this example, we first include the necessary header files, `iostream` for input and output, and `fstream` for file operations. Then, we use the `using namespace std;` statement to avoid typing `std::` for every variable and function from the standard library.

Next, we create an `ifstream` object, which is used to open and read text files. Using the `open()` function, we specify the name of the text file we want to read. In this case, we use a sample text file called "sample.txt".

We then check if the file was successfully opened using the `if` statement. If there was an error, we notify the user and exit the program. Otherwise, we continue with reading the file line by line using the `getline()` function. This function takes two parameters, the file object and a string variable where each line of the file will be stored.

Once all the lines have been read, we close the file using the `close()` function.

Running this code will output the contents of the text file to the console.

```
This is a sample text file.
It contains multiple lines.
Each line will be output to the console.
```

## Deep Dive

In the previous example, we used the `getline()` function to read the text file line by line. However, there are other ways to read a file in C++ using the `fstream` header file.

For example, the `get()` function can be used to read a single character from the file, while the `read()` function can be used to read a specific number of characters.

Additionally, we can also use the `get()` and `read()` functions to read data from binary files, while the `write()` function can be used to write data to a file.

It is important to note that when opening a file to read, we use the `ifstream` object, while for writing to a file, we use the `ofstream` object. Both of these are derived from the base class `fstream`.

## See Also

- [C++ Input/Output](https://www.programiz.com/cpp-programming/library-function/cstdio/fscanf)
- [fstream documentation](http://www.cplusplus.com/reference/fstream/)
- [Learn C++ File Handling](https://www.geeksforgeeks.org/basics-file-handling-c/)

Reading and writing text files in C++ is a fundamental skill that every programmer should possess. With the help of the `fstream` header file, you can easily perform various file operations in your programs. Make sure to practice and experiment with different file handling techniques to become proficient in this essential aspect of C++ programming.