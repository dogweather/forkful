---
title:                "C++ recipe: Reading a text file"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a fundamental skill for any programmer. It allows us to access and manipulate data stored in external files, making our programs more dynamic and versatile. Learning how to read a text file in C++ is a crucial step towards becoming a proficient coder.

## How To

To read a text file in C++, we first need to include the `fstream` library, which provides the necessary tools for working with files. Then, we can use the `ifstream` (input file stream) object to open the file and read its contents.

Let's take a look at an example:

```C++

#include <iostream>
#include <fstream>

int main(){
    //Declare variables
    std::string line;
    std::ifstream inputFile;
    
    //Open file for reading
    inputFile.open("example.txt");

    //Read file line by line
    while (std::getline(inputFile, line)){
        std::cout << line << std::endl;
    }

    //Close file
    inputFile.close();

    return 0;
}
```

In this code, we first declare a `string` variable to store the content of each line in the file. Then, we create an `ifstream` object and use its `open()` function to open the desired file. Next, we use a `while` loop to read the file line by line using `getline()` and print each line to the console. Finally, we close the file with the `close()` function.

Let's say our "example.txt" file contains the following lines:

```
Hello
This is a text file
12345
```

The output of our code would be:

```
Hello
This is a text file
12345
```

## Deep Dive

There are a few important things to keep in mind when reading a text file in C++. The first is error handling. We should always check if the file has been successfully opened before attempting to read from it. This can be done by using the `is_open()` function on our `ifstream` object. Additionally, we can use the `fail()` function to check if any errors occur during reading.

Another point to remember is that `getline()` reads each line until it encounters a line break character (`'\n'`), so it will not read the last line of a file if it doesn't end with a line break. To avoid this, we can use `while (getline(inputFile, line))` instead of `while (!inputFile.eof())` to ensure all lines are read.

Lastly, we can also specify the delimiter for `getline()` by passing a character as the third argument. For example, `getline(inputFile, line, ',')` will read the file until it reaches a comma and then stop.

## See Also

For more information on the `fstream` library and working with files in C++, check out these helpful resources:

- [C++ File Input/Output](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Reading and Writing Files in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [C++ Reference - `fstream` library](https://www.cplusplus.com/reference/fstream/)