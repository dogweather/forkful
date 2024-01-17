---
title:                "Reading a text file"
html_title:           "C++ recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in C++ refers to the process of accessing and extracting data from a file that contains text. Programmers do this to read and manipulate the contents of the text file, such as retrieving specific information or modifying the existing data.

## How to:

To read a text file in C++, you can use the ```ifstream``` class from the standard library. First, you need to include the ```<fstream>``` header file in your code.

Next, create an ```ifstream``` object and specify the name of the text file you want to read. For example, if the file is named "data.txt", you can declare the object as:

```C++
ifstream file("data.txt");
```

You can then use a loop to read each line of the file using the ```getline()``` function, which takes two parameters - the stream object and a string variable to store the read data. Here's an example code:

```C++
string line;
while(getline(file, line)) {
    // manipulate the line data here
}
```

You can also use the extraction operator ```>>``` to read individual data elements from the file. For example, if your file contains integer values separated by spaces, you can read them as:

```C++
int num;
while(file >> num) {
    // manipulate the integer values here
}
```

Remember to close the file once you're done reading by calling the ```close()``` function on the ```ifstream``` object.

## Deep Dive

Historically, text file reading in C++ used the ```fopen()``` function from C. With the release of C++11, the ```ifstream``` class provides a more convenient and C++ oriented way of reading text files.

An alternative to using ```ifstream``` is the ```fstream``` class, which can be used to both read and write to files. However, for reading purposes, using ```ifstream``` is more efficient and recommended.

To read binary files, you can use the ```ifstream::read()``` function, which takes a buffer and the number of bytes to read as parameters.

## See Also

- [C++ ifstream documentation](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [C++ ofstream documentation](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- [C++ fstream documentation](https://en.cppreference.com/w/cpp/io/basic_fstream)
- [C file input/output](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)